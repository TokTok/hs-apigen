{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
module Apigen.Types
    ( BitSize (..)
    , BuiltinType (..)
    , Constness (..)
    , Generated (..)
    , Decl
        ( Typename
        , TypeDecl
        , BuiltinType
        , ConstType
        , PointerType
        , ConstPointerType
        , CallbackType

        , ArrayType
        , ConstArrayType
        , UserArrayType
        , SizedArrayType

        , ClassDecl
        , Namespace

        , CallbackTypeDecl
        , IdTypeDecl

        , Constructor
        , Destructor
        , Method
        , Property
        , ValueProp
        , ArrayProp

        , Function
        , Define
        , Var

        , Ref
        , IntVal
        , Paren
        , Abs
        , Max
        , Add
        , Sub
        , Mul
        , Div

        , EnumMember
        , Enumeration
        )
    , Module (Module)
    , Model (Model)
    ) where

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Text     (Text)

data BitSize
    = B8
    | B16
    | B32
    | B64
    deriving (Show, Eq)
$(deriveJSON defaultOptions ''BitSize)

data BuiltinType
    = Void
    | VoidPtr
    | Bool
    | Char
    | SInt BitSize
    | UInt BitSize
    | SizeT
    | String
    deriving (Show, Eq)
$(deriveJSON defaultOptions ''BuiltinType)

data Constness
    = ConstThis
    | MutableThis
    deriving (Show, Eq)
$(deriveJSON defaultOptions ''Constness)

data Generated
    = GeneratedToString
    | GeneratedFromInt
    deriving (Show, Eq)
$(deriveJSON defaultOptions ''Generated)

data Decl lexeme
    = Typename { name :: lexeme }
    | TypeDecl { name :: lexeme }
    | BuiltinType { bty :: BuiltinType }
    | ConstType { ty :: Decl lexeme }
    | PointerType { name :: lexeme }
    | ConstPointerType { name :: lexeme }
    | CallbackType { name :: lexeme }

    | ArrayType { bty :: BuiltinType }
    | ConstArrayType { bty :: BuiltinType }
    | UserArrayType { name :: lexeme }
    | SizedArrayType { memTy :: Decl lexeme, sizer :: Decl lexeme }

    | ClassDecl { name :: lexeme, mems :: [Decl lexeme] }
    | Namespace { ns :: [Text], mems :: [Decl lexeme] }

    | CallbackTypeDecl { name :: lexeme, params :: [Decl lexeme] }
    | IdTypeDecl { name :: lexeme }

    | Constructor { name :: lexeme, params :: [Decl lexeme] }
    | Destructor { name :: lexeme, params :: [Decl lexeme] }
    | Method { constness :: Constness, ty :: Decl lexeme, name :: lexeme, params :: [Decl lexeme] }
    | Property { name :: lexeme, prop :: Decl lexeme }
    | ValueProp
        { valType :: Decl lexeme
        , valGet  :: Maybe (Decl lexeme)
        , valSet  :: Maybe (Decl lexeme)
        }
    | ArrayProp
        { arrType :: Decl lexeme
        , arrGet  :: Maybe (Decl lexeme)
        , arrSet  :: Maybe (Decl lexeme)
        , arrSize :: Maybe (Decl lexeme)
        }

    | Function { retTy :: Decl lexeme, name :: lexeme, params :: [Decl lexeme] }
    | Define { name :: lexeme }
    | Var { ty :: Decl lexeme, name :: lexeme }

    | Ref { name :: lexeme }
    | IntVal { val :: lexeme }
    | Paren { expr :: Decl lexeme }
    | Abs { expr :: Decl lexeme }
    | Max { left :: Decl lexeme, right :: Decl lexeme }
    | Add { left :: Decl lexeme, right :: Decl lexeme }
    | Sub { left :: Decl lexeme, right :: Decl lexeme }
    | Mul { left :: Decl lexeme, right :: Decl lexeme }
    | Div { left :: Decl lexeme, right :: Decl lexeme }

    | EnumMember { name :: lexeme }
    | Enumeration { gen :: [Generated], name :: lexeme, mems :: [Decl lexeme] }
    deriving (Show, Functor, Foldable, Traversable, Eq)
$(deriveJSON defaultOptions ''Decl)

data Module lexeme = Module { file :: FilePath, decls :: [Decl lexeme] }
    deriving (Show, Functor, Foldable, Traversable)
$(deriveJSON defaultOptions ''Module)

newtype Model lexeme = Model { mods :: [Module lexeme] }
    deriving (Show, Functor, Foldable, Traversable)
$(deriveJSON defaultOptions ''Model)
