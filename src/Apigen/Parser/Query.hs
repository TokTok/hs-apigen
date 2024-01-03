{-# LANGUAGE Strict #-}
module Apigen.Parser.Query (declName) where

import           Apigen.Parser.SymbolTable (SId, Sym)
import           Apigen.Types              (Decl (..))
import           Language.Cimple           (Lexeme)

declName :: Sym -> Maybe (Lexeme SId)
declName (TypeDecl name)           = Just name
declName (Function _ name _)       = Just name
declName (Method _ _ name _)       = Just name
declName (Enumeration _ name _)    = Just name
declName (CallbackTypeDecl name _) = Just name
declName (IdTypeDecl name)         = Just name
declName (Define name)             = Just name
declName (Var _ name)              = Just name
-- ignore properties, ctors, dtors, namespaces, and classes, we don't want to namespace them further
declName Property{}                = Nothing
declName Constructor{}             = Nothing
declName Destructor{}              = Nothing
declName Namespace{}               = Nothing
declName ClassDecl{}               = Nothing
declName x                         = error $ "unhandled in declName: " <> show x
