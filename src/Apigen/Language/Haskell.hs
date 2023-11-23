{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}
module Apigen.Language.Haskell where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Maybe                  (catMaybes)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), LexemeClass (..),
                                              Node, NodeF (..))
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import qualified Text.Casing                 as Casing


joinLines :: [Text] -> Text
joinLines = Text.intercalate "\n"


idToHaskell :: Text -> Text
idToHaskell =
    Text.pack
    . Casing.toPascal
    . Casing.Identifier
    . dropNamespace
    . Casing.unIdentifier
    . Casing.fromSnake
    . Text.unpack
  where
    -- Drop the first component of the name, but only if there are at least
    -- 2 components.
    dropNamespace (_:name@(_:_)) = name
    dropNamespace name           = name


maybeParen :: Text -> Text
maybeParen name
    | ' ' `elem` Text.unpack name = "(" <> name <> ")"
    | otherwise                   = name


isErrorEnum :: Text -> Bool
isErrorEnum tyName =
    case Casing.unIdentifier . Casing.fromSnake . Text.unpack $ tyName of
        (_:"Err":_) -> True
        _           -> False


genStdType :: Text -> Text
genStdType "uint16_t" = "Word16"
genStdType "uint32_t" = "Word32"
genStdType "uint64_t" = "Word64"
genStdType "size_t"   = "CSize"
genStdType "bool"     = "Bool"
genStdType tyName     = error $ show tyName


genType :: Node (Lexeme Text) -> Text
genType                               (Fix (TyUserDefined (L _ IdSueType tyName))) =
    "CEnum " <> idToHaskell tyName
genType (Fix (TyPointer               (Fix (TyUserDefined (L _ IdSueType tyName)))  ))
    | isErrorEnum tyName = "CErr " <> idToHaskell tyName
    | otherwise = idToHaskell tyName <> "Ptr"

genType (Fix (TyPointer (Fix (TyConst (Fix (TyUserDefined (L _ IdSueType tyName))))))) =
    idToHaskell tyName <> "Ptr"
genType (Fix (TyPointer (Fix (TyConst (Fix (TyStruct (L _ IdSueType tyName))))))) =
    idToHaskell tyName <> "Ptr"
genType (Fix (TyPointer               (Fix (TyStruct (L _ IdSueType tyName)))  )) =
    idToHaskell tyName <> "Ptr"

genType (Fix (TyPointer               (Fix (TyStd (L _ IdStdType "uint8_t")))  )) =
    "CString"
genType (Fix (TyPointer (Fix (TyConst (Fix (TyStd (L _ IdStdType "uint8_t"))))))) =
    "CString"
genType (Fix (TyPointer (Fix (TyConst (Fix (TyStd (L _ IdStdType "char"))))))) =
    "CString"

genType (Fix (TyPointer (Fix (TyStd (L _ KwVoid _))))) =
    "Ptr ()"
genType (Fix (TyPointer (Fix (TyFunc (L _ IdFuncType tyName))))) =
    "FunPtr " <> idToHaskell tyName
genType                 (Fix (TyStd (L _ IdStdType tyName))) =
    genStdType tyName
genType (Fix (TyPointer (Fix (TyStd (L _ IdStdType tyName))))) =
    "Ptr " <> genStdType tyName
genType (Fix (TyStd (L _ KwVoid _))) =
    "()"
genType ty = error $ show ty


genArg :: Node (Lexeme Text) -> Text
genArg (Fix (VarDecl ty _ _)) = genType ty
genArg arg                    = error $ show arg


genArgs :: [Node (Lexeme Text)] -> Text
genArgs args = Text.intercalate " -> " (map genArg args) <> " -> "


genFunction :: Node (Lexeme Text) -> Text -> [Node (Lexeme Text)] -> Text
genFunction retTy name [(Fix (TyStd (L _ KwVoid _)))] =
    "foreign import ccall " <> name <> " :: " <> genType retTy
genFunction retTy name args =
    "foreign import ccall " <> name <> " :: " <> genArgs args <> "IO " <> maybeParen (genType retTy)


genFuncType :: Node (Lexeme Text) -> Text -> [Node (Lexeme Text)] -> Text
genFuncType retTy name args = joinLines
    [ "type " <> hsName <> " = " <> genArgs args <> "IO " <> maybeParen (genType retTy)
    , "foreign import ccall \"wrapper\" wrap" <> hsName <> " :: " <> hsName <> " -> IO (FunPtr " <> hsName <> ")"
    ]
  where
    hsName = idToHaskell name


genEnumerator :: Node (Lexeme Text) -> Maybe Text
genEnumerator (Fix Comment{})                   = Nothing
genEnumerator (Fix (Enumerator (L _ _ name) _)) = Just $ idToHaskell name
genEnumerator x                                 = error $ show x


genEnum :: Text -> [Node (Lexeme Text)] -> Text
genEnum name enums = joinLines $
    [ "data " <> idToHaskell name
    , "    = " <> Text.intercalate "\n    | " (catMaybes (map genEnumerator enums))
    , "    deriving (Eq, Ord, Enum, Bounded, Read, Show)"
    ]


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \_file node act ->
        case unFix node of
            FunctionDecl _ (Fix (FunctionPrototype retTy (L _ _ name) args)) ->
                State.modify (genFunction retTy name args:)
            TypedefFunction (Fix (FunctionPrototype retTy (L _ _ name) args)) ->
                State.modify (genFuncType retTy name args:)
            EnumDecl (L _ _ name) enums _ ->
                State.modify (genEnum name enums:)

            _ -> act
    }


generate :: (FilePath, [Node (Lexeme Text)]) -> (FilePath, Text)
generate = ("file.hs",) . joinLines . reverse . flip State.execState [] . traverseAst linter
