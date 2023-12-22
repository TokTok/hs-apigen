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


genEnumerator :: Bool -> Node (Lexeme Text) -> Maybe Text
genEnumerator isErr (Fix (Enumerator (L _ _ name) _))
    -- Skip the OK error, because we process that in CErr handling.
    | isErr && "_OK" `Text.isSuffixOf` name = Nothing
    | otherwise                             = Just $ idToHaskell name
genEnumerator _ (Fix Comment{}) = Nothing
genEnumerator _ x = error $ show x


genEnum :: Text -> [Node (Lexeme Text)] -> Text
genEnum name enums = joinLines $
    [ "data " <> hsName
    , "    = " <> Text.intercalate "\n    | " (catMaybes (map (genEnumerator (isErrorEnum name)) enums))
    , "    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)"
    , "instance MessagePack " <> hsName
    , "instance Arbitrary " <> hsName <> " where arbitrary = arbitraryBoundedEnum"
    ]
  where
    hsName = idToHaskell name


genStruct :: Text -> Text
genStruct name = joinLines $
    [ "data " <> structName
    , "type " <> ptrName <> " = Ptr " <> structName
    ]
  where
    structName = idToHaskell name <> "Struct"
    ptrName    = idToHaskell name <> "Ptr"


generator :: AstActions (State [Text]) Text
generator = astActions
    { doNode = \_file node act ->
        case unFix node of
            FunctionDecl _ (Fix (FunctionPrototype retTy (L _ _ name) args)) ->
                State.modify (genFunction retTy name args:)
            Struct (L _ _ name) _ ->
                State.modify (genStruct name:)
            Typedef _ (L _ _ name) ->
                State.modify (genStruct name:)
            TypedefFunction (Fix (FunctionPrototype retTy (L _ _ name) args)) ->
                State.modify (genFuncType retTy name args:)
            EnumDecl (L _ _ name) enums _ ->
                State.modify (genEnum name enums:)
            EnumConsts (Just (L _ _ name)) enums ->
                State.modify (genEnum name enums:)

            _ -> act
    }


addPrologue :: FilePath -> [Text] -> [Text]
addPrologue file = (
    [ "{-# LANGUAGE DeriveGeneric #-}"
    , "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    , "module " <> hsModuleName file <> " where"
    , ""
    , "import           Data.MessagePack          (MessagePack)"
    , "import           Data.Word                 (Word16, Word32, Word64)"
    , "import           Foreign.C.Enum            (CEnum (..), CErr)"
    , "import           Foreign.C.String          (CString)"
    , "import           Foreign.C.Types           (CInt (..), CSize (..))"
    , "import           Foreign.Ptr               (FunPtr, Ptr)"
    , "import           GHC.Generics              (Generic)"
    , "import           Test.QuickCheck.Arbitrary (Arbitrary (..),"
    , "                                            arbitraryBoundedEnum)"
    , if "tox/tox_events.h" `Text.isSuffixOf` Text.pack file then "import FFI.Tox.Tox" else ""
    ]++)
  where
    hsModuleName =
        ("FFI."<>)
        . Text.intercalate "."
        . map (Text.pack . Casing.toPascal . Casing.Identifier . (:[]) . Text.unpack)
        . reverse . take 2 . reverse  -- takeEnd from extra
        . Text.splitOn "/"
        . Text.dropEnd 2
        . Text.pack


generate :: (FilePath, [Node (Lexeme Text)]) -> (FilePath, Text)
generate input@(file, _) = go input
  where
    go = (file,) . joinLines . addPrologue file . reverse . flip State.execState [] . traverseAst generator
