{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TupleSections     #-}
module Apigen.Parser.InferSizedGet (simplify) where

import           Apigen.Parser.Query       (declName)
import           Apigen.Parser.SymbolTable (Name, SId, SIdToName, Sym,
                                            mustLookup)
import           Apigen.Types              (Constness (ConstThis), Decl (..))
import           Control.Arrow             (Arrow (first), (&&&))
import           Data.List                 (isSuffixOf)
import qualified Data.List                 as List
import           Data.Maybe                (mapMaybe)
import           Data.Text                 (Text)
import           Language.Cimple           (Lexeme, lexemeText)

simplify :: SIdToName -> [Sym] -> (SIdToName, [Sym])
simplify st decls = (st, map go decls)
  where
    go :: Sym -> Sym
    go (Namespace ns mems) =
        Namespace ns $ map (inject st names . go) mems
      where
        names = mapMaybe (hoistMaybe . first getterForSize) . mapMaybe (fmap (mustLookup st . lexemeText &&& id) . declName) $ mems
    go (ClassDecl ns mems) =
        ClassDecl ns $ map (inject st names . go) mems
      where
        names = mapMaybe (hoistMaybe . first getterForSize) . mapMaybe (fmap (mustLookup st . lexemeText &&& id) . declName) $ mems

    go x = x

inject :: SIdToName -> [([Text], Lexeme SId)] -> Sym -> Sym
inject st names sym =
    case flip List.lookup names . snd . mustLookup st . lexemeText =<< declName sym of
        Just sizer -> makeArraySized sizer sym
        _          -> sym

makeArraySized :: Lexeme SId -> Sym -> Sym
makeArraySized sizer (Method ConstThis ret name params) =
    Method ConstThis ret name $ map go params
  where
    go :: Sym -> Sym
    go (Var (ArrayType     ty) var) = Var (SizedArrayType (BuiltinType ty) (Ref sizer)) var
    go (Var (UserArrayType ty) var) = Var (SizedArrayType (Typename    ty) (Ref sizer)) var
    go var = var
makeArraySized _ sym = sym

getterForSize :: Name -> Maybe [Text]
getterForSize (_, name@("get":_)) | ["size"] `isSuffixOf` name = Just (take (length name - 1) name)
getterForSize _  = Nothing

hoistMaybe :: (Maybe a, b) -> Maybe (a, b)
hoistMaybe (a, b) = (,b) <$> a
