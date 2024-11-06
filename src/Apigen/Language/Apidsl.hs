{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict     #-}
module Apigen.Language.Apidsl where

import           Apigen.Parser.SymbolTable (Name, display, displayWithin)
import           Apigen.Types              (BitSize (..), BuiltinType (..),
                                            Constness (..), Decl (..),
                                            Generated (..), Model (..),
                                            Module (..))
import           Data.Maybe                (maybeToList)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as TL
import           Language.Cimple           (Lexeme (..), lexemeText)
import           Prettyprinter
import           Prettyprinter.Render.Text as Term

type Context = [Text]

alwaysNamespace :: Bool
alwaysNamespace = True

commaSpace :: Doc ()
commaSpace = comma <> softline

ppModel :: Model (Lexeme Name) -> Doc ()
ppModel (Model mods) = vcat (map ppModule mods) <> line

ppModule :: Module (Lexeme Name) -> Doc ()
ppModule (Module file decls) =
    pretty "from \"" <> pretty (file <> "\"") <$$> line <> vcat (map (ppDecl []) decls)

ppFunction :: Context -> Lexeme Name -> [Decl (Lexeme Name)] -> Doc ()
ppFunction ctx name params =
    ppLexeme ctx name <> lparen <> align (hcat (punctuate commaSpace $ map (ppDecl ctx) params)) <> rparen

ppDecl :: Context -> Decl (Lexeme Name) -> Doc ()
ppDecl ctx = \case
    Namespace name mems ->
        nest 2 (
            pretty "namespace" <+> pretty (Text.unpack (Text.intercalate (Text.pack " ") name)) <+> lbrace <$$>
            vcat (map (ppDecl (ctx ++ name)) mems)
        ) <$$> rbrace

    ClassDecl name mems ->
        nest 2 (
            pretty "class" <+> ppLexeme ctx name <+> lbrace <$$>
            vcat (map (ppDecl ctx) mems)
        ) <$$> rbrace <> semi

    Enumeration funs name mems ->
        nest 2 (
            pretty "enum" <+> ppLexeme ctx name <+> lbracket <>
            hcat (punctuate commaSpace (map ppGenerated funs))
            <> rbracket <+> lbrace <$$>
            vcat (map (ppDecl ctx) mems)
        ) <$$> rbrace <> semi

    Property name prop ->
        nest 2 (
            pretty "property" <+> ppLexeme ctx name <+> colon <+> ppDecl ctx prop
        ) <$$> rbrace <> semi
    ValueProp valType valGet valSet ->
        ppDecl ctx valType <+> lbrace <$$>
        vcat (map (ppDecl ctx) (maybeToList valGet ++ maybeToList valSet))
    ArrayProp arrType arrGet arrSet arrSize ->
        ppDecl ctx arrType <+> lbrace <$$>
        vcat (map (ppDecl ctx) (maybeToList arrGet ++ maybeToList arrSet ++ maybeToList arrSize))

    Method constness ret name params ->
        pretty "method" <+> ppDecl ctx ret <+> ppFunction ctx name params <+> ppConstness constness <> semi
    Function ret name params ->
        pretty "function" <+> ppDecl ctx ret <+> ppFunction ctx name params <> semi
    Constructor name params ->
        pretty "constructor" <+> ppFunction ctx name params <> semi
    Destructor name params ->
        pretty "destructor" <+> ppFunction ctx name params <> semi
    CallbackTypeDecl name params ->
        pretty "callback" <+> ppFunction ctx name params <> semi
    IdTypeDecl name -> pretty "typedef uint32_t" <+> ppLexeme ctx name
    TypeDecl name -> pretty "typedef struct" <+> ppLexeme ctx name <+> ppLexeme ctx name <> semi

    Var ty name ->
        ppDecl ctx ty <+> ppLexeme ctx name
    Define name ->
        pretty "const" <+> ppLexeme ctx name <> semi

    Typename name -> ppLexeme ctx name
    EnumMember name -> ppLexeme ctx name <> comma
    BuiltinType ty -> ppBuiltinType ty
    CallbackType ty -> ppLexeme ctx ty
    PointerType ty -> ppLexeme ctx ty <> pretty '*'
    ConstPointerType ty -> pretty "const" <+> ppLexeme ctx ty <> pretty '*'
    SizedArrayType ty name -> ppDecl ctx ty <> lbracket <> ppDecl ctx name <> rbracket
    ArrayType ty -> ppBuiltinType ty <> lbracket <> pretty '?' <> rbracket
    UserArrayType ty -> ppLexeme ctx ty <> lbracket <> pretty '?' <> rbracket
    ConstArrayType ty -> pretty "const" <+> ppBuiltinType ty <> lbracket <> pretty '?' <> rbracket
    ConstType ty -> pretty "const" <+> ppDecl ctx ty

    Paren expr -> lparen <> ppDecl ctx expr <> rparen
    Ref name -> ppLexeme ctx name
    IntVal val -> ppLexeme ctx val
    Abs e -> pretty "abs" <> lparen <> ppDecl ctx e <> rparen
    Max a b -> pretty "max" <> lparen <> ppDecl ctx a <> comma <+> ppDecl ctx b <> rparen
    Add l r -> ppDecl ctx l <+> pretty '+' <+> ppDecl ctx r
    Sub l r -> ppDecl ctx l <+> pretty '-' <+> ppDecl ctx r
    Mul l r -> ppDecl ctx l <+> pretty '*' <+> ppDecl ctx r
    Div l r -> ppDecl ctx l <+> pretty '/' <+> ppDecl ctx r

ppGenerated :: Generated -> Doc ()
ppGenerated GeneratedToString = pretty "to_string"
ppGenerated GeneratedFromInt  = pretty "from_int"

ppConstness :: Constness -> Doc ()
ppConstness ConstThis   = pretty "const"
ppConstness MutableThis = pretty "mutable"

ppBuiltinType :: BuiltinType -> Doc ()
ppBuiltinType Void      = pretty "void"
ppBuiltinType VoidPtr   = pretty "void*"
ppBuiltinType Bool      = pretty "bool"
ppBuiltinType Char      = pretty "pretty"
ppBuiltinType (SInt bs) = pretty "int" <> ppBitSize bs <> pretty "_t"
ppBuiltinType (UInt bs) = pretty "uint" <> ppBitSize bs <> pretty "_t"
ppBuiltinType SizeT     = pretty "size_t"
ppBuiltinType String    = pretty "string"

ppBitSize :: BitSize -> Doc ()
ppBitSize B8  = int 8
ppBitSize B16 = int 16
ppBitSize B32 = int 32
ppBitSize B64 = int 64

ppMaybe :: (a -> Doc ()) -> Maybe a -> Doc ()
ppMaybe _ Nothing  = mempty
ppMaybe f (Just x) = f x

ppLexeme :: Context -> Lexeme Name -> Doc ()
ppLexeme ctx = pretty . nonEmpty . (if alwaysNamespace then display else displayWithin ctx) . lexemeText
  where
    nonEmpty "" = "this"
    nonEmpty t  = t

renderSmart :: Float -> Int -> Doc () -> SimpleDocStream ()
renderSmart ribbonFraction widthPerLine
    = layoutSmart LayoutOptions
        { layoutPageWidth = AvailablePerLine widthPerLine (realToFrac ribbonFraction) }

renderS :: Doc () -> String
renderS = Text.unpack . render

render :: Doc () -> Text
render = TL.toStrict . Term.renderLazy . renderSmart 1 120

generate :: Model (Lexeme Name) -> Text
generate = render . ppModel

infixr 5 <$$>
(<$$>) :: Doc a -> Doc a -> Doc a
x <$$> y = x <> line <> y

int :: Int -> Doc ()
int = pretty
