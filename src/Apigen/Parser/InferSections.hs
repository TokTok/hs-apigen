{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TupleSections     #-}
module Apigen.Parser.InferSections (simplify) where

import           Apigen.Parser.Query        (declName)
import           Apigen.Parser.SymbolTable  (M, Name, SId, SIdToName, Sym,
                                             mustLookup, renameM)
import           Apigen.Types               (Decl (..))
import           Control.Arrow              (Arrow (first, second), (&&&))
import           Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import           Data.Bifunctor             (Bifunctor (bimap))
import           Data.List                  (find, isPrefixOf, nub, sortOn)
import qualified Data.List                  as List
import           Data.List.Extra            (groupOn)
import           Data.Maybe                 (mapMaybe)
import           Data.Ord                   (Down (Down))
import qualified Data.Text                  as Text
import           Data.Tuple                 (swap)
import           Language.Cimple            (lexemeText)

namespaceNames :: [(Maybe (SId, Name), Sym)] -> [Name]
namespaceNames =
    nub
    . map (second (takeWhile (not . (`elem` propertyPrefixes))))
    . filter hasProperty
    . filter (not . isSpecial)
    . mapMaybe (fmap (second (map Text.toLower) . snd) . fst)

propertyPrefixes :: [Text.Text]
propertyPrefixes = ["get", "set"]

hasProperty :: Name -> Bool
hasProperty = flip any propertyPrefixes . flip elem . snd

isSpecial :: Name -> Bool
isSpecial = flip any prefixes . flip isPrefixOf . snd
  where prefixes = map (:[]) $ "callback":"err":"Err":propertyPrefixes

simplify :: SIdToName -> [Sym] -> (SIdToName, [Sym])
simplify st = first fst . swap . flip State.runState (st, ()) . mapM go
  where
    go :: Sym -> M () Sym
    go (Namespace name mems) = Namespace name <$> mapM go mems

    go (ClassDecl name mems) = do
        stripNamespacesM namespaced
        return $ ClassDecl name (unnamespaced ++ map mkNamespace namespaced)
      where
        mkNamespace = uncurry Namespace . bimap snd (map snd)

        -- Each class member mapped to its declaration name SId and Name.
        names = map (first (fmap $ id &&& mustLookup st) . (fmap lexemeText . declName &&& id)) mems
        nsNames = namespaceNames names
        nss = sortOn (maybe Nothing (`List.elemIndex` nsNames) . fst) -- Restore original order.
              . collectByFst
              . map (select . sortOn (Down . length . snd) $ nsNames)
              $ names

        unnamespaced = maybe [] (map snd) . List.lookup Nothing $ nss
        namespaced = mapMaybe (\(ns, mem) -> (,mem) <$> ns) nss

    go x = return x

stripNamespacesM :: [(Name, [(Maybe (SId, Name), Sym)])] -> State (SIdToName, ()) ()
stripNamespacesM =
    mapM_ (uncurry (mapM_ . renameM . dropAfterErr . length . snd) . second (mapMaybe (fmap fst . fst)))
  where
    dropAfterErr n (ns,x@"Err":xs)      = (ns++getNs n xs, x:drop n xs)
    dropAfterErr n (ns,x@"err":xs)      = (ns++getNs n xs, x:drop n xs)
    dropAfterErr n (ns,x@"callback":xs) = (ns++getNs n xs, x:drop n xs)
    dropAfterErr n (ns,xs)              = (ns++getNs n xs,   drop n xs)

    getNs n xs = map Text.toLower (take n xs)

select :: [Name] -> (Maybe (SId, Name), Sym) -> (Maybe Name, (Maybe (SId, Name), Sym))
select nsNames x =
    ((\name -> find (second (map Text.toLower) name `isInNamespace`) nsNames) . snd =<< fst x, x)

isInNamespace :: Name -> Name -> Bool
isInNamespace (_,"callback":name) = (`isPrefixOf` name) . snd
isInNamespace (_,"err":name)      = (`isPrefixOf` name) . snd
isInNamespace (_,name)            = (`isPrefixOf` name) . snd

collectByFst :: Ord a => [(a, b)] -> [(a, [b])]
collectByFst = map collapse . groupOn fst . sortOn fst
  where
    collapse xs@(x:_) = (fst x, map snd xs)
    collapse _        = error "collect: empty list unexpected"
