{-# LANGUAGE Strict #-}
module Apigen.Parser.InferNamespace (simplify) where

import           Apigen.Parser.Query       (declName)
import           Apigen.Parser.SymbolTable (Name, SId, SIdToName, Sym,
                                            mustLookup)
import           Apigen.Types              (Decl (..))
import           Control.Arrow             (second, (&&&))
import           Data.Bifunctor            (Bifunctor (bimap))
import qualified Data.HashMap.Strict       as HashMap
import           Data.Maybe                (mapMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Language.Cimple           (lexemeText)

commonPrefix :: Eq a => [[a]] -> [a]
commonPrefix [] = []
commonPrefix (first:rest) = foldl go first rest
  where
    go _ [] = []
    go [] _ = []
    go (x:xs) (y:ys)
      | x == y    = x : go xs ys
      | otherwise = []

simplify :: SIdToName -> [Sym] -> (SIdToName, [Sym])
simplify st decls = (renamed, [Namespace namespace decls])
  where
    sids :: [(SId, Name)]
    sids = map (id &&& mustLookup st) $ mapMaybe (fmap lexemeText . declName) decls

    namespace :: [Text]
    namespace = commonPrefix . map (map Text.toLower . snd . snd) $ sids

    renamed = foldr (uncurry HashMap.insert . (second . bimap (const namespace) . drop . length $ namespace)) st sids
