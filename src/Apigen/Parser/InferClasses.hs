{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# OPTIONS_GHC -Wwarn #-}
module Apigen.Parser.InferClasses (simplify) where

import           Apigen.Parser.SymbolTable  (M, Name, SId, SIdToName, Sym,
                                             display, mustLookupM, renameM)
import           Apigen.Types               (Constness (..), Decl (..))
import           Control.Arrow              (Arrow (first, second))
import           Control.Monad              ((>=>))
import           Control.Monad.Extra        (mapMaybeM)
import qualified Control.Monad.State.Strict as State
import           Data.Bifunctor             (Bifunctor (bimap))
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.List                  (sortOn)
import qualified Data.List                  as List
import qualified Data.Maybe                 as Maybe
import           Data.Ord                   (Down (Down))
import qualified Data.Text                  as Text
import           Data.Tuple                 (swap)
import           Language.Cimple            (Lexeme (..))

type NameTable = HashMap Name Sym

insertClass :: SId -> Sym -> M NameTable ()
insertClass sid sym = do
    name <- mustLookupM sid
    State.modify $ second $ HashMap.insert (second (map Text.toLower) name) sym

prefixes :: Name -> HashMap Name a -> [(Name, a)]
prefixes name =
    sortOn (Down . sum . map Text.length . snd . fst)
    . filter ((`List.isPrefixOf` snd name) . snd . fst)
    . HashMap.toList

matchesThis :: Maybe (Lexeme SId) -> Lexeme SId -> Bool
matchesThis Nothing _                        = False
matchesThis (Just (L _ _ this)) (L _ _ name) = this == name

data MemberType
    = Static
    | Member
    | New
    | Free

insert :: Name -> Lexeme SId -> [Sym] -> (MemberType -> Sym) -> MemberType -> M NameTable ()
insert k clsName mems sym =
    State.modify . second . HashMap.insert k . ClassDecl clsName . (mems ++) . (:[]) . sym

insertMember :: Maybe (Lexeme SId) -> Lexeme SId -> (MemberType -> Sym) -> M NameTable (Maybe Sym)
insertMember this (L _ _ sid) sym = do
    syms <- snd <$> State.get
    name <- mustLookupM sid
    let errPrefix = Maybe.fromMaybe [] . List.find (`List.isPrefixOf` snd name) $ [["Err"], ["err"]]
    case prefixes (second (map Text.toLower . drop (length errPrefix)) name) syms of
        (k, ClassDecl clsName mems):_ -> do
            -- No need to check for commonality here. The prefixes function already did.
            -- Put back the Err/err prefix if we had one.
            let renamed = bimap (++ map Text.toLower (snd k)) ((errPrefix ++) . drop (length errPrefix + length (snd k))) name
            renameM (const renamed) sid
            insert k clsName mems sym $ case snd renamed of
                -- TODO(iphydf): Check return type.
                ["new"]  -> New
                ["derive"]  -> New
                ["derive","with","salt"]  -> New
                -- TODO(iphydf): Check first param type.
                ["free"] -> Free
                ["kill"] -> Free
                _ | null this -> Static
                _ | matchesThis this clsName -> Member
                _ ->
                    error $ show this <> " `this` is not the correct namespace for " <> display k <> " renamed " <> display renamed
            return Nothing
        (k, _):_ -> do
            sname <- mustLookupM sid
            error $ "cannot insert " <> display sname <> " into " <> display k
        [] -> return $ Just $ sym Static

insertMethod :: Sym -> Lexeme SId -> Maybe (Lexeme SId) -> [Sym] -> Constness -> M NameTable (Maybe Sym)
insertMethod ret name this params constness =
    insertMember this name $ \case
        New    -> Constructor name params
        Free   -> Destructor name (tail params)
        Member -> Method constness ret name (tail params)
        Static -> Function ret name params

inject :: [Sym] -> M NameTable [Sym]
inject xs = (xs ++) . HashMap.elems . snd <$> State.get

simplify :: SIdToName -> [Sym] -> (SIdToName, [Sym])
simplify st = first fst . swap . flip State.runState (st, HashMap.empty) . (mapMaybeM createClasses >=> mapMaybeM go)
  where
    createClasses :: Sym -> M NameTable (Maybe Sym)
    createClasses (Namespace ns mems) =
        Just . Namespace ns <$> mapMaybeM createClasses mems
    createClasses (TypeDecl l@(L _ _ name)) = do
        insertClass name (ClassDecl l [])
        return Nothing
    createClasses x = return $ Just x

    go :: Sym -> M NameTable (Maybe Sym)
    go (Namespace ns mems) =
        Just . Namespace ns <$> (mapMaybeM go mems >>= inject)

    go (Function ret name params@(Var (PointerType this) _:_)) = do
        insertMethod ret name (Just this) params MutableThis
    go (Function ret name params@(Var (ConstPointerType this) _:_)) = do
        insertMethod ret name (Just this) params ConstThis
    go (Function ret@PointerType{} name params) = do
        insertMethod ret name Nothing params MutableThis
    go decl@(Function _ name _) = do
        insertMember Nothing name $ const decl
    go decl@(Enumeration _ name _) = do
        insertMember Nothing name $ const decl
    go decl@(CallbackTypeDecl name _) = do
        insertMember Nothing name $ const decl
    go decl@(IdTypeDecl name) = do
        insertMember Nothing name $ const decl
    go decl@(Define name) = do
        insertMember Nothing name $ const decl
    go x = error $ "unhandled in InferClasses: " <> show x
