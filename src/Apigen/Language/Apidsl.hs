{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{- HLINT ignore "Functor law" -}
{- HLINT ignore "Use <$" -}
module Apigen.Language.Apidsl where

import           Apigen.Parser.SymbolTable    (Name, display, displayWithin)
import           Apigen.Types                 (BitSize (..), BuiltinType (..),
                                               Constness (..), Decl (..),
                                               Generated (..), Model (..),
                                               Module (..))
import           Data.Maybe                   (maybeToList)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Language.Cimple              (Lexeme (..), lexemeText)
import           Prelude                      hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

type Context = [Text]

alwaysNamespace :: Bool
alwaysNamespace = True

commaSpace :: Doc
commaSpace = comma <> softline

ppModel :: Model (Lexeme Name) -> Doc
ppModel (Model mods) = vcat (map ppModule mods) <> line

ppModule :: Module (Lexeme Name) -> Doc
ppModule (Module file decls) =
    text "from \"" <> text file <> "\"" <$> line <> vcat (map (ppDecl []) decls)

ppFunction :: Context -> Lexeme Name -> [Decl (Lexeme Name)] -> Doc
ppFunction ctx name params =
    ppLexeme ctx name <> lparen <> align (hcat (punctuate commaSpace $ map (ppDecl ctx) params)) <> rparen

ppDecl :: Context -> Decl (Lexeme Name) -> Doc
ppDecl ctx = \case
    Namespace name mems ->
        nest 2 (
            text "namespace" <+> text (Text.unpack (Text.intercalate "_" name)) <+> lbrace <$>
            vcat (map (ppDecl (ctx ++ name)) mems)
        ) <$> rbrace

    ClassDecl name mems ->
        nest 2 (
            text "class" <+> ppLexeme ctx name <+> lbrace <$>
            vcat (map (ppDecl ctx) mems)
        ) <$> rbrace <> semi

    Enumeration funs name mems ->
        nest 2 (
            text "enum" <+> ppLexeme ctx name <+> lbracket <>
            hcat (punctuate commaSpace (map ppGenerated funs))
            <> rbracket <+> lbrace <$>
            vcat (map (ppDecl ctx) mems)
        ) <$> rbrace <> semi

    Property name prop ->
        nest 2 (
            text "property" <+> ppLexeme ctx name <+> colon <+> ppDecl ctx prop
        ) <$> rbrace <> semi
    ValueProp valType valGet valSet ->
        ppDecl ctx valType <+> lbrace <$>
        vcat (map (ppDecl ctx) (maybeToList valGet ++ maybeToList valSet))
    ArrayProp arrType arrGet arrSet arrSize ->
        ppDecl ctx arrType <+> lbrace <$>
        vcat (map (ppDecl ctx) (maybeToList arrGet ++ maybeToList arrSet ++ maybeToList arrSize))

    Method constness ret name params ->
        text "method" <+> ppDecl ctx ret <+> ppFunction ctx name params <+> ppConstness constness <> semi
    Function ret name params ->
        text "function" <+> ppDecl ctx ret <+> ppFunction ctx name params <> semi
    Constructor name params ->
        text "constructor" <+> ppFunction ctx name params <> semi
    Destructor name params ->
        text "destructor" <+> ppFunction ctx name params <> semi
    CallbackTypeDecl name params ->
        text "callback" <+> ppFunction ctx name params <> semi
    IdTypeDecl name -> text "typedef uint32_t" <+> ppLexeme ctx name
    TypeDecl name -> "typedef struct" <+> ppLexeme ctx name <+> ppLexeme ctx name <> semi

    Var ty name ->
        ppDecl ctx ty <+> ppLexeme ctx name
    Define name ->
        text "const" <+> ppLexeme ctx name <> semi

    Typename name -> ppLexeme ctx name
    EnumMember name -> ppLexeme ctx name <> comma
    BuiltinType ty -> ppBuiltinType ty
    CallbackType ty -> ppLexeme ctx ty
    PointerType ty -> ppLexeme ctx ty <> char '*'
    ConstPointerType ty -> text "const" <+> ppLexeme ctx ty <> char '*'
    SizedArrayType ty name -> ppDecl ctx ty <> lbracket <> ppDecl ctx name <> rbracket
    ArrayType ty -> ppBuiltinType ty <> lbracket <> char '?' <> rbracket
    UserArrayType ty -> ppLexeme ctx ty <> lbracket <> char '?' <> rbracket
    ConstArrayType ty -> "const" <+> ppBuiltinType ty <> lbracket <> char '?' <> rbracket
    ConstType ty -> text "const" <+> ppDecl ctx ty

    Paren expr -> lparen <> ppDecl ctx expr <> rparen
    Ref name -> ppLexeme ctx name
    IntVal val -> ppLexeme ctx val
    Abs e -> text "abs" <> lparen <> ppDecl ctx e <> rparen
    Max a b -> text "max" <> lparen <> ppDecl ctx a <> comma <+> ppDecl ctx b <> rparen
    Add l r -> ppDecl ctx l <+> char '+' <+> ppDecl ctx r
    Sub l r -> ppDecl ctx l <+> char '-' <+> ppDecl ctx r
    Mul l r -> ppDecl ctx l <+> char '*' <+> ppDecl ctx r
    Div l r -> ppDecl ctx l <+> char '/' <+> ppDecl ctx r

ppGenerated :: Generated -> Doc
ppGenerated GeneratedToString = text "to_string"
ppGenerated GeneratedFromInt  = text "from_int"

ppConstness :: Constness -> Doc
ppConstness ConstThis   = text "const"
ppConstness MutableThis = text "mutable"

ppBuiltinType :: BuiltinType -> Doc
ppBuiltinType Void      = text "void"
ppBuiltinType VoidPtr   = text "void*"
ppBuiltinType Bool      = text "bool"
ppBuiltinType Char      = text "char"
ppBuiltinType (SInt bs) = text "int" <> ppBitSize bs <> text "_t"
ppBuiltinType (UInt bs) = text "uint" <> ppBitSize bs <> text "_t"
ppBuiltinType SizeT     = text "size_t"
ppBuiltinType String    = text "string"

ppBitSize :: BitSize -> Doc
ppBitSize B8  = int 8
ppBitSize B16 = int 16
ppBitSize B32 = int 32
ppBitSize B64 = int 64

ppMaybe :: (a -> Doc) -> Maybe a -> Doc
ppMaybe _ Nothing  = empty
ppMaybe f (Just x) = f x

ppLexeme :: Context -> Lexeme Name -> Doc
ppLexeme ctx = text . nonEmpty . (if alwaysNamespace then display else displayWithin ctx) . lexemeText
  where
    nonEmpty "" = "this"
    nonEmpty t  = t

renderS :: Doc -> String
renderS = flip displayS "" . renderSmart 1 120

render :: Doc -> Text
render = Text.pack . renderS

generate :: Model (Lexeme Name) -> Text
generate = render . ppModel
