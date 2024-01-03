{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Apigen.Parser.InferGenerated (simplify) where

import           Apigen.Parser.SymbolTable  (M, Name, SIdToName, Sym, display,
                                             mustLookupM)
import           Apigen.Types               (Decl (..), Generated (..))
import           Control.Arrow              (Arrow (first, second))
import           Control.Monad.Extra        (concatMapM)
import qualified Control.Monad.State.Strict as State
import           Data.List                  (isPrefixOf)
import qualified Data.Text                  as Text
import           Data.Tuple                 (swap)
import           Language.Cimple            (Lexeme (..))

simplify :: SIdToName -> [Sym] -> (SIdToName, [Sym])
simplify st = first fst . swap . flip State.runState (st, Nothing) . concatMapM go
  where
    go :: Sym -> M (Maybe (Name, Sym)) [Sym]
    go (Namespace name mems) = do
        (leftovers, mems') <- descend mems
        return $ leftovers ++ [Namespace name mems']
    go (ClassDecl name mems) = do
        (leftovers, mems') <- descend mems
        return $ leftovers ++ [ClassDecl name mems']

    go e@(Enumeration _ (L _ _ sid) _) = do
        ctx <- snd <$> State.get
        case ctx of
            Nothing -> do
                name <- second (map Text.toLower) <$> mustLookupM sid
                State.modify $ second $ const $ Just (name, e)
                return []
            Just (_, esym) -> do
                State.modify $ second $ const Nothing
                (esym:) <$> go e

    go f@(Function _ (L _ _ sid) _) = do
        name <- mustLookupM sid
        ctx <- snd <$> State.get
        case ctx of
            -- No enum currently being processed.
            Nothing -> return [f]
            Just (ename, Enumeration funs esid mems) | snd ename `isPrefixOf` snd name -> do
                case reverse $ snd name of
                    ("string":"to":_) -> do
                        State.modify $ second $ const $ Just (ename, Enumeration (GeneratedToString:funs) esid mems)
                        return []
                    _ ->
                        error $ "invalid enum function: " <> display name
            Just (_, esym) -> do
                State.modify $ second $ const Nothing
                (esym:) <$> go f

    go x = do
        ctx <- snd <$> State.get
        case ctx of
            Nothing -> return [x]
            Just (_, esym) -> do
                State.modify $ second $ const Nothing
                (esym:) <$> go x


    descend mems = do
        ctx <- snd <$> State.get
        leftovers <- case ctx of
            Nothing -> return []
            Just (_, esym) -> do
                State.modify $ second $ const Nothing
                return [esym]
        State.modify $ second $ const Nothing
        res <- concatMapM go mems
        ctx' <- snd <$> State.get
        case ctx' of
            Nothing -> return (leftovers, res)
            Just (_, esym) -> do
                State.modify $ second $ const Nothing
                return (leftovers, res ++ [esym])
