{-# LANGUAGE Strict #-}
module Apigen.Parser.InferSizedParam (simplify) where

import           Apigen.Parser.SymbolTable (SIdToName, Sym)
import           Apigen.Types              (BuiltinType (SizeT), Decl (..))

simplify :: SIdToName -> [Sym] -> (SIdToName, [Sym])
simplify st decls = (st, map go decls)
  where
    go :: Sym -> Sym
    go (Namespace ns mems) = Namespace ns $ map go mems

    go (Function ret     name params) = Function ret     name (reverse . foldl collapse [] $ params)
    go (CallbackTypeDecl name params) = CallbackTypeDecl name (reverse . foldl collapse [] $ params)
    go x = x

collapse :: [Sym] -> Sym -> [Sym]
collapse [] p = [p]
collapse (Var (ConstArrayType ty) name:ps) (Var (BuiltinType SizeT) len) =
    Var (SizedArrayType (ConstType (BuiltinType ty)) (Ref len)) name:ps
collapse (Var (ArrayType ty) name:ps) (Var (BuiltinType SizeT) len) =
    Var (SizedArrayType (BuiltinType ty) (Ref len)) name:ps
collapse ps p = p:ps
