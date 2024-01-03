{-# OPTIONS_GHC -Wwarn -fmax-pmcheck-models=100 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
module Apigen.Parser.InferProperties (simplify) where

import           Apigen.Parser.Query        (declName)
import           Apigen.Parser.SymbolTable  (M, Name, SId, SIdToName, Sym,
                                             display, mustLookupM, resolve)
import qualified Apigen.Parser.SymbolTable  as SymbolTable
import           Apigen.Types               (BuiltinType (..), Constness (..),
                                             Decl (..))
import           Control.Arrow              (Arrow (first, second))
import           Control.Monad              ((>=>))
import           Control.Monad.Extra        (mapMaybeM)
import qualified Control.Monad.State.Strict as State
import qualified Data.Foldable              as Foldable
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import           Data.List                  (isSuffixOf)
import           Data.Maybe                 (maybeToList)
import           Data.Text                  (Text)
import           Data.Tuple                 (swap)
import           GHC.Stack                  (HasCallStack)
import           Language.Cimple            (Lexeme (..), lexemeText)

type Prop = Decl (Lexeme SId)
type PropTable = InsOrdHashMap Name Prop

data Kind
    = KindGet
    | KindSet
    | KindSize
    deriving (Show)

addSymbols :: M PropTable (InsOrdHashMap SId Prop)
addSymbols = do
    props <- snd <$> State.get
    fmap InsOrdHashMap.fromList . mapM insert . InsOrdHashMap.toList $ props
  where
    insert :: (Name, Prop) -> M PropTable (SId, Prop)
    insert (name, prop) =
        (,prop) <$> SymbolTable.insert name


propSloc :: Show lexeme => Decl lexeme -> [lexeme]
propSloc prop = case prop of
    ValueProp t g s   -> go [Just t, g, s   ]
    ArrayProp t g s l -> go [Just t, g, s, l]
    _                 -> error $ show prop
  where go = concatMap (concat . maybeToList . fmap Foldable.toList)


simplify :: SIdToName -> [Sym] -> (SIdToName, [Sym])
simplify st = first fst . swap . flip State.runState (st, InsOrdHashMap.empty) . mapMaybeM go
  where
    go :: Sym -> M PropTable (Maybe Sym)
    go (Namespace name mems) = Just . Namespace name <$> descend mems
    go (ClassDecl name mems) = Just . ClassDecl name <$> descend mems

    go m@(Method _ _ (L _ _ sid) _) = do
        name <- mustLookupM sid
        mth name m

    go x = return $ Just x

    descend mems = do
        old <- snd <$> State.get
        State.modify $ second $ const InsOrdHashMap.empty
        newMems <- mapMaybeM go mems
        props <- map (uncurry Property . near) . InsOrdHashMap.toList <$> addSymbols
        State.modify $ second $ const old
        return $ props ++ newMems

    near :: (SId, Prop) -> (Lexeme SId, Prop)
    near (t, x@(head . propSloc -> L c p _)) = (L c p t, x)

    -- Ignore these non-compliant functions.
    mth :: HasCallStack => Name -> Sym -> M PropTable (Maybe Sym)
    mth (_, ["get","savedata","data"]) (Method ConstThis _ _ _) = return Nothing
    mth (_, ["get","savedata","length"]) (Method ConstThis _ _ _) = return Nothing
    mth (_, ["set","savedata","length"]) (Method MutableThis _ _ _) = return Nothing

    -- Ignore log callback for now. We'll use Tox_Log later.
    mth (_, ["get","log","callback"]) (Method ConstThis _ _ _) = return Nothing
    mth (_, ["set","log","callback"]) (Method MutableThis _ _ _) = return Nothing
    mth (_, ["get","log","user","data"]) (Method ConstThis _ _ _) = return Nothing
    mth (_, ["set","log","user","data"]) (Method MutableThis _ _ _) = return Nothing

    -- Ignore Tox_System for now.
    mth (_, ["get","operating","system"]) (Method ConstThis _ _ _) = return Nothing
    mth (_, ["set","operating","system"]) (Method MutableThis _ _ _) = return Nothing
    mth (_, ["get","system"]) (Method MutableThis _ _ _) = return Nothing

    -- Ignore toxav_get_tox for now.
    mth (_, ["get","tox"]) (Method ConstThis _ _ _) = return Nothing

    mth (ns, "get":name) m@(Method ConstThis (BuiltinType SizeT) _ _) | ["size"] `isSuffixOf` name = do
        -- We might not know the type, yet.
        State.modify $ second $ addArrayProp KindSize (BuiltinType Void) (ns, take (length name - 1) name) m
        return Nothing
    mth (ns, "get":name) m@(Method ConstThis ret _ params) = do
        case findPropertyParam st name params of
            Just ty@(SizedArrayType BuiltinType{} _) ->
                State.modify $ second $ addArrayProp KindGet ty (ns,name) m
            Just ty | isArrayType ty ->
                State.modify $ second $ addArrayProp KindGet ty (ns,name) m
            Nothing | isValueType ret ->
                State.modify $ second $ addValueProp st KindGet ret (ns,name) m
            Just ty -> error $ "found a property getter for unsupported type: " <> show (resolve st ty)
            Nothing -> error $ "did not find property parameter for " <> display (ns,name) <> ": " <> show (resolve st m)
        return Nothing
    mth (ns, "set":name) m@(Method MutableThis _ _ params) = do
        case findPropertyParam st name params of
            Just (SizedArrayType (ConstType ty@BuiltinType{}) size) ->
                State.modify $ second $ addArrayProp KindSet (SizedArrayType ty size) (ns,name) m
            Just (ConstArrayType ty) ->
                State.modify $ second $ addArrayProp KindSet (ArrayType ty) (ns,name) m
            Just ty | isValueType ty ->
                State.modify $ second $ addValueProp st KindSet ty (ns,name) m
            Just ty -> error $ "found a property setter for unsupported type: " <> show (resolve st ty)
            Nothing -> error $ "did not find property parameter for " <> display (ns,name) <> ": " <> show (resolve st m)
        return Nothing

    mth (ns, "set":name) (Method ConstThis _ _ _) =
        error $ "setter for " <> display (ns,name) <> " has a const `this`"

    mth (_, "get":_) m@Method{} = error $ "invalid getter format: " <> show (resolve st m)
    mth (_, "set":_) m@Method{} = error $ "invalid setter format: " <> show (resolve st m)

    mth _ m = return $ Just m

findPropertyParam :: SIdToName -> [Text] -> [Sym] -> Maybe Sym
findPropertyParam st name =
    Foldable.find isProperty >=> getVarType
  where
    isProperty = (Just name ==) . fmap (snd . SymbolTable.mustLookup st . lexemeText) . declName

    getVarType (Var ty _) = Just ty
    getVarType _          = Nothing

isValueType :: Decl lexeme -> Bool
isValueType (BuiltinType SInt{}) = True
isValueType (BuiltinType UInt{}) = True
isValueType (BuiltinType Bool)   = True
isValueType (BuiltinType String) = True
isValueType Typename{}           = True
isValueType _                    = False

isArrayType :: Decl lexeme -> Bool
isArrayType ArrayType{}      = True
isArrayType UserArrayType{}  = True
isArrayType SizedArrayType{} = True
isArrayType _                = False

addValueProp :: HasCallStack => SIdToName -> Kind -> Sym -> Name -> Sym -> PropTable -> PropTable
addValueProp st kind ty name mth syms =
    InsOrdHashMap.insert name prop' syms
  where
    prop' =
        case (kind, prop) of
            (KindGet, ValueProp valTy Nothing set) -> ValueProp valTy (Just mth) set
            (KindSet, ValueProp valTy get Nothing) -> ValueProp valTy get (Just mth)
            _ -> error $ "accessor of type " <> show kind <> " already present for value property " <> show name <> ": " <> show (mth, resolve st prop)
    prop =
        case InsOrdHashMap.lookup name syms of
            Nothing  -> ValueProp ty Nothing Nothing
            Just acc -> acc


addArrayProp :: HasCallStack => Kind -> Sym -> Name -> Sym -> PropTable -> PropTable
addArrayProp kind ty name mth syms =
    InsOrdHashMap.insert name prop' syms
  where
    prop' =
        case (kind, prop) of
            (KindGet, ArrayProp arrTy Nothing set size) -> ArrayProp arrTy (Just mth) set size
            (KindSet, ArrayProp arrTy get Nothing size) -> ArrayProp arrTy get (Just mth) size
            (KindSize, ArrayProp arrTy get set Nothing) -> ArrayProp arrTy get set (Just mth)
            _ -> error $ "accessor of type " <> show kind <> " already present for array property " <> show name <> ": " <> show (mth, prop)
    prop =
        case InsOrdHashMap.lookup name syms of
            Nothing  -> ArrayProp ty Nothing Nothing Nothing
            -- If we didn't know it yet, maybe we do now.
            Just (ArrayProp (BuiltinType Void) get set size) -> ArrayProp ty get set size
            Just acc -> acc
