{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict     #-}
module Apigen.Language.PyDsl (generate) where

import           Apigen.Parser.SymbolTable (Name)
import           Apigen.Types              (BitSize (..), BuiltinType (..),
                                            Constness (..), Decl (..),
                                            Generated (..), Model (..),
                                            Module (..))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as TL
import           Language.Cimple           (Lexeme (..), LexemeClass)
import           Prelude                   hiding ((<$>))
import           Prettyprinter
import           Prettyprinter.Render.Text as Term

commaSpace :: Doc ()
commaSpace = comma <> softline

commaSep :: [Doc ()] -> [Doc ()]
commaSep = punctuate commaSpace

go :: ([Doc ()] -> Doc ()) -> String -> [Doc ()] -> Doc ()
go f cls mems = ppCtor cls <> lparen <> f (commaSep mems) <> rparen

ppCtor :: String -> Doc ()
ppCtor = pretty . ("apigen." <>)

hgo :: String -> [Doc ()] -> Doc ()
hgo = go hcat

vgo :: String -> [Doc ()] -> Doc ()
vgo = go vcat

linebreak :: Doc ()
linebreak = group (flatAlt line mempty)

ppList :: (a -> Doc ()) -> [a] -> Doc ()
ppList pp l = lbracket <> linebreak <> hcat (commaSep (map pp l)) <> rbracket

ppModel :: Model (Lexeme Name) -> Doc ()
ppModel (Model mods) =
    vgo "Model" [ppList ppModule mods] <> line

ppModule :: Module (Lexeme Name) -> Doc ()
ppModule (Module file decls) =
    vgo "Module" [pretty "\"" <> pretty file <> pretty "\"", ppList ppDecl decls]

ppDecl :: Decl (Lexeme Name) -> Doc ()
ppDecl = \case
    Namespace name mems                     -> vgo "Namespace" [ppList (pretty . show) name, ppList ppDecl mems]
    ClassDecl name mems                     -> vgo "ClassDecl" [ppLexeme name, ppList ppDecl mems]
    Enumeration funs name mems              -> vgo "Enumeration" [ppList ppGenerated funs, ppLexeme name, ppList ppDecl mems]
    Property name prop                      -> hgo "Property" [ppLexeme name, ppDecl prop]
    ValueProp valType valGet valSet         -> hgo "ValueProp" [ppDecl valType, ppMaybe ppDecl valGet, ppMaybe ppDecl valSet]
    ArrayProp arrType arrGet arrSet arrSize -> hgo "ArrayProp" [ppDecl arrType, ppMaybe ppDecl arrGet, ppMaybe ppDecl arrSet, ppMaybe ppDecl arrSize]
    Method constness ret name params        -> hgo "Method" [ppConstness constness, ppDecl ret, ppLexeme name, ppList ppDecl params]
    Function ret name params                -> hgo "Function" [ppDecl ret, ppLexeme name, ppList ppDecl params]
    Constructor name params                 -> hgo "Constructor" [ppLexeme name, ppList ppDecl params]
    Destructor name params                  -> hgo "Destructor" [ppLexeme name, ppList ppDecl params]
    CallbackTypeDecl name params            -> hgo "CallbackTypeDecl" [ppLexeme name, ppList ppDecl params]
    IdTypeDecl name                         -> hgo "IdTypeDecl" [ppLexeme name]
    TypeDecl name                           -> hgo "TypeDecl" [ppLexeme name]
    Var ty name                             -> hgo "Var" [ppDecl ty, ppLexeme name]
    Define name                             -> hgo "Define" [ppLexeme name]
    Typename name                           -> hgo "Typename" [ppLexeme name]
    EnumMember name                         -> hgo "EnumMember" [ppLexeme name]
    BuiltinType ty                          -> hgo "BuiltinType" [ppBuiltinType ty]
    CallbackType ty                         -> hgo "CallbackType" [ppLexeme ty]
    PointerType ty                          -> hgo "PointerType" [ppLexeme ty]
    ConstPointerType ty                     -> hgo "ConstPointerType" [ppLexeme ty]
    SizedArrayType ty name                  -> hgo "SizedArrayType" [ppDecl ty, ppDecl name]
    ArrayType ty                            -> hgo "ArrayType" [ppBuiltinType ty]
    UserArrayType ty                        -> hgo "UserArrayType" [ppLexeme ty]
    ConstArrayType ty                       -> hgo "ConstArrayType" [ppBuiltinType ty]
    ConstType ty                            -> hgo "ConstType" [ppDecl ty]
    Paren expr                              -> hgo "Paren" [ppDecl expr]
    Ref name                                -> hgo "Ref" [ppLexeme name]
    IntVal val                              -> hgo "IntVal" [ppLexeme val]
    Abs e                                   -> hgo "Abs" [ppDecl e]
    Max a b                                 -> hgo "Max" [ppDecl a, ppDecl b]
    Add l r                                 -> hgo "Add" [ppDecl l, ppDecl r]
    Sub l r                                 -> hgo "Sub" [ppDecl l, ppDecl r]
    Mul l r                                 -> hgo "Mul" [ppDecl l, ppDecl r]
    Div l r                                 -> hgo "Div" [ppDecl l, ppDecl r]

ppConstness :: Constness -> Doc ()
ppConstness ConstThis   = pretty "True"
ppConstness MutableThis = pretty "False"

ppGenerated :: Generated -> Doc ()
ppGenerated = ppCtor . ("Generated." <>) . show

ppBuiltinType :: BuiltinType -> Doc ()
ppBuiltinType Void      = ppCtor "Void"
ppBuiltinType VoidPtr   = ppCtor "VoidPtr"
ppBuiltinType Bool      = ppCtor "Bool"
ppBuiltinType Char      = ppCtor "Char"
ppBuiltinType (SInt bs) = hgo "SInt" [ppBitSize bs]
ppBuiltinType (UInt bs) = hgo "UInt" [ppBitSize bs]
ppBuiltinType SizeT     = ppCtor "SizeT"
ppBuiltinType String    = ppCtor "String"

ppBitSize :: BitSize -> Doc ()
ppBitSize B8  = int 8
ppBitSize B16 = int 16
ppBitSize B32 = int 32
ppBitSize B64 = int 64

ppMaybe :: (a -> Doc ()) -> Maybe a -> Doc ()
ppMaybe _ Nothing  = pretty "None"
ppMaybe f (Just x) = f x

ppLexeme :: Lexeme Name -> Doc ()
ppLexeme (L _ c s) = ppName c s

ppName :: LexemeClass -> Name -> Doc ()
ppName c (ns, name) = hgo "Name"
    [ ppCtor . ("LexemeClass." <>) . show $ c
    , ppList (pretty . show) ns
    , ppList (pretty . show) name
    ]


renderSmart :: Float -> Int -> Doc () -> SimpleDocStream ()
renderSmart ribbonFraction widthPerLine
    = layoutSmart LayoutOptions
        { layoutPageWidth = AvailablePerLine widthPerLine (realToFrac ribbonFraction) }

render :: Doc () -> Text
render = TL.toStrict . Term.renderLazy . renderSmart 1 120

generate :: Model (Lexeme Name) -> Text
generate = render . ppModel

int :: Int -> Doc ()
int = pretty
