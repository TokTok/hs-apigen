{-# OPTIONS_GHC -Wwarn -fmax-pmcheck-models=100 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Apigen.Parser (parseModel) where

import qualified Apigen.Parser.InferClasses    as InferClasses
import qualified Apigen.Parser.InferGenerated  as InferGenerated
import qualified Apigen.Parser.InferNamespace  as InferNamespace
import qualified Apigen.Parser.InferProperties as InferProperties
import qualified Apigen.Parser.InferSections   as InferSections
import qualified Apigen.Parser.InferSizedGet   as InferSizedGet
import qualified Apigen.Parser.InferSizedParam as InferSizedParam
import qualified Apigen.Parser.SymbolNumbers   as SymbolNumbers
import           Apigen.Parser.SymbolTable     (M, Name, SId, SIdToName, Sym,
                                                display, mustLookupM, renameM,
                                                resolveM)
import           Apigen.Patterns
import           Apigen.Types                  (BitSize (..), BuiltinType (..),
                                                Decl (..), Model (..),
                                                Module (..))
import           Control.Arrow                 (Arrow (first, second))
import           Control.Monad                 ((>=>))
import           Control.Monad.Extra           (concatMapM)
import           Control.Monad.State.Strict    (State)
import qualified Control.Monad.State.Strict    as State
import           Data.Fix                      (foldFixM)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.Cimple               (BinaryOp (..), Lexeme (..),
                                                LexemeClass (..),
                                                LiteralType (ConstId, Int),
                                                Node, NodeF (..), Scope (..),
                                                lexemeText)

parseModel :: [SymbolNumbers.TranslationUnit Text] -> Model (Lexeme Name)
parseModel = Model . uncurry mods . SymbolNumbers.collect
  where
    mods tus = State.evalState $ mapM (uncurry parseModule) tus >>= mapM resolveM

parseModule :: FilePath -> [Node (Lexeme SId)] -> M a (Module (Lexeme SId))
parseModule f = fmap (Module f) . (concatMapM (foldFixM go) >=> runSimplify)

runSimplify :: [Sym] -> State (SIdToName, a) [Sym]
runSimplify decls = do
    (st, simplified) <- simplify . second (const decls) <$> State.get
    State.modify $ first $ const st
    return simplified
  where
    simplify = flip (foldr uncurry)
        [ (,)
        , InferGenerated.simplify
        , InferProperties.simplify
        , InferSizedGet.simplify
        , InferSections.simplify
        , InferClasses.simplify
        , InferSizedParam.simplify
        , InferNamespace.simplify
        ]

go :: NodeF (Lexeme SId) [Sym] -> M a [Sym]
-- {-
go (PreprocInclude _) = return []
go (TyPointer [ConstType (BuiltinType UInt{})]) = return []
go (TyPointer [           BuiltinType UInt{} ]) = return []
go (VarDecl [] _ []) = return []
go (FunctionPrototype [] _ _) = return []

go (PreprocIfndef (L _ _ SYM_APIGEN_IGNORE) _ es) = return es
go (PreprocIfndef (L _ _ SYM_TOX_HIDE_DEPRECATED) _ es) = return es

go (FunctionPrototype [ret] name [[BuiltinType Void]]) = return [Function ret name []]
go (FunctionPrototype [ret] name params              ) = return [Function ret name (concat params)]

go (FunctionDecl Global func) = return func
go (TypedefFunction [Function (BuiltinType Void) name params]) = return [CallbackTypeDecl name params]
go (Typedef         [BuiltinType (UInt B32)]     name        ) = return [IdTypeDecl name]

go (Enumerator name _) = return [EnumMember name]
go (EnumConsts (Just name) enums  ) = mkEnum name enums
go (EnumDecl         name  enums _) = mkEnum name enums

go (TyPointer [ConstType (BuiltinType Char)]) = return [BuiltinType String]
go (TyPointer [           BuiltinType Char ]) = return [BuiltinType String]
go (TyPointer [           Typename ty      ]) = return [     PointerType ty]
go (TyPointer [ConstType (Typename ty     )]) = return [ConstPointerType ty]

go (DeclSpecArray (Just [expr]))                                  = return [SizedArrayType (BuiltinType Void) expr]
go (DeclSpecArray Nothing)                                        = return [ArrayType Void]
go (VarDecl [           BuiltinType ty ] name [[ArrayType Void]]) = return [Var (ArrayType      ty     ) name]
go (VarDecl [ConstType (BuiltinType ty)] name [[ArrayType Void]]) = return [Var (ConstArrayType ty     ) name]
go (VarDecl [Typename ty               ] name [[ArrayType Void]]) = return [Var (UserArrayType  ty     ) name]
go (VarDecl [ty] name [[SizedArrayType (BuiltinType Void) size]]) = return [Var (SizedArrayType ty size) name]

go (FunctionCall [Ref (L _ _ SYM_abs)] [[expr]  ]) = return [Abs expr]
go (FunctionCall [Ref (L _ _ SYM_max)] [[a], [b]]) = return [Max a b]

-- -}

go (TyConst [ty])                  = return [ConstType ty]
go (TyPointer [BuiltinType Void])  = return [BuiltinType VoidPtr]
go (TyPointer ty@[CallbackType{}]) = return ty

go (AggregateDecl ty@[TypeDecl _]) = return ty
go (Struct ty _)                   = return [TypeDecl ty]
go (TyStruct ty)                   = return [Typename ty]
go (TyUserDefined ty)              = return [Typename ty]
go (TyFunc ty)                     = return [CallbackType ty]
go (Typedef [Typename ty] _)       = return [TypeDecl ty]

go (TyStd (L _ _ TY_void    ))     = return [BuiltinType Void]
go (TyStd (L _ _ TY_char    ))     = return [BuiltinType Char]
go (TyStd (L _ _ TY_bool    ))     = return [BuiltinType Bool]
go (TyStd (L _ _ TY_int8_t  ))     = return [BuiltinType (SInt B8)]
go (TyStd (L _ _ TY_uint8_t ))     = return [BuiltinType (UInt B8)]
go (TyStd (L _ _ TY_int16_t ))     = return [BuiltinType (SInt B16)]
go (TyStd (L _ _ TY_uint16_t))     = return [BuiltinType (UInt B16)]
go (TyStd (L _ _ TY_int32_t ))     = return [BuiltinType (SInt B32)]
go (TyStd (L _ _ TY_uint32_t))     = return [BuiltinType (UInt B32)]
go (TyStd (L _ _ TY_int64_t ))     = return [BuiltinType (SInt B64)]
go (TyStd (L _ _ TY_uint64_t))     = return [BuiltinType (UInt B64)]
go (TyStd (L _ _ TY_size_t  ))     = return [BuiltinType SizeT]

go (PreprocDefineConst name _)     = return [Define name]
go (VarDecl [ty] name [])          = return [Var ty name]
go (VarExpr name)                  = return [Ref name]
go (ParenExpr [x])                 = return [Paren x]
go (LiteralExpr ConstId name)      = return [Ref name]
go (LiteralExpr Int val)           = return [IntVal val]
go (BinaryExpr [l] BopPlus  [r])   = return [Add l r]
go (BinaryExpr [l] BopMinus [r])   = return [Sub l r]
go (BinaryExpr [l] BopMul   [r])   = return [Mul l r]
go (BinaryExpr [l] BopDiv   [r])   = return [Div l r]

go BinaryExpr{}                    = return []
go CopyrightDecl{}                 = return []
go LicenseDecl{}                   = return []
go Comment{}                       = return []
go CommentSectionEnd{}             = return []
go CommentInfo{}                   = return []
go PreprocDefine{}                 = return []
go SizeofType{}                    = return []
-- TODO(iphydf): Create bindings for public structs?
go MemberDecl{}                    = return []
-- TODO(iphydf): Create bindings for macros?
go MacroBodyFunCall{}              = return []
go PreprocDefineMacro{}            = return []
go ParenExpr{}                     = return []
go FunctionCall{}                  = return []
go (PreprocIfndef _ ts es)         = return $ concat ts ++ es
go (PreprocElse xs)                = return $ concat xs
go (Group xs)                      = return $ concat xs
go (Commented _ x)                 = return x
go (CommentSection _ xs _)         = return $ concat xs
go (ExternC xs)                    = return $ concat xs
go Ellipsis                        = return []

go x                               = error $ "parse failed: " <> show x

mkEnum :: Lexeme SId -> [[Sym]] -> State (SIdToName, a) [Sym]
mkEnum name enums = (:[]) . Enumeration [] name <$> ((stripNamespace . lexemeText $ name) . concat $ enums)

stripNamespace :: SId -> [Sym] -> M a [Sym]
stripNamespace ns decls = do
    as <- mustLookupM ns
    (mapM_ . mapM_ . mapM_) (renameM (dropCommon as)) decls
    return decls

dropCommon :: Name -> Name -> Name
dropCommon (an,a:as) (bn,b:bs) | Text.toLower a == Text.toLower b = dropCommon (an,as) (bn,bs)
dropCommon (_,[]) bs = bs
dropCommon as bs = error $ display bs <> " is not in the namespace of " <> display as
