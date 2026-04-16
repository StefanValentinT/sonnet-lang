{-# LANGUAGE TupleSections #-}

module Typer (infer) where

import Builtins
import Control.Monad (foldM)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import SimpleType
import Simplify
import Syntax
import System.Mem.StableName
import TyperHelpers

data TypedTermI
    = TIIdent String SimpleType
    | TILitInt Int
    | TIApp TypedTermI TypedTermI SimpleType
    | TIFun String TypedTermI SimpleType
    | TILetIn String TypedTermI TypedTermI SimpleType
    | TIRecord (Map String TypedTermI) SimpleType -- Changed to Map
    | TISel TypedTermI String SimpleType
    | TIIs TypedTermI Type TypedTermI TypedTermI SimpleType
    | TIIf TypedTermI TypedTermI TypedTermI SimpleType

type Context = Map String SimpleType

typeTerm :: Term -> Context -> IO (TypedTermI, SimpleType)
typeTerm t ctx = case t of
    LitInt n -> return (TILitInt n, SPrimI32)
    Ident n -> case Map.lookup n ctx of
        Just ty -> return (TIIdent n ty, ty)
        Nothing -> error $ "Unbound variable: " ++ n
    Fun name body -> do
        param <- freshVar
        (tBody, resTy) <- typeTerm body (Map.insert name param ctx)
        let selfTy = SFunction param resTy
        return (TIFun name tBody selfTy, selfTy)
    App f a -> do
        res <- freshVar
        (tf, tF) <- typeTerm f ctx
        (ta, tA) <- typeTerm a ctx
        constrain tF (SFunction tA res)
        return (TIApp tf ta res, res)
    LetIn name rhs body -> do
        if name == "_"
            then do
                (tr, tR) <- typeTerm rhs ctx
                (tb, tB) <- typeTerm body ctx
                return (TILetIn "_" tr tb tB, tB)
            else do
                p <- freshVar
                let recCtx = Map.insert name p ctx
                (tr, tR) <- typeTerm rhs recCtx
                constrain tR p
                (tb, tB) <- typeTerm body recCtx
                return (TILetIn name tr tb tB, tB)
    Record fs -> do
        res <- mapM (`typeTerm` ctx) fs
        let tms = Map.map fst res
        let tys = Map.map snd res
        let selfTy = SRecord tys
        return (TIRecord tms selfTy, selfTy)
    Sel r name -> do
        res <- freshVar
        (tr, tR) <- typeTerm r ctx
        constrain tR (SRecord (Map.singleton name res))
        return (TISel tr name res, res)
    If c th el -> do
        (tc, tC) <- typeTerm c ctx
        constrain tC SPrimBool
        (t1, tT) <- typeTerm th ctx
        (t2, tE) <- typeTerm el ctx
        res <- freshVar
        constrain tT res
        constrain tE res
        return (TIIf tc t1 t2 res, res)
    Is v ty th el -> do
        (tv, tV) <- typeTerm v ctx
        (t1, tT) <- typeTerm th ctx
        (t2, tE) <- typeTerm el ctx
        res <- freshVar
        constrain tT res
        constrain tE res
        return (TIIs tv ty t1 t2 res, res)

data OrdSN = OrdSN (StableName SimpleType) deriving (Eq)
instance Ord OrdSN where compare (OrdSN a) (OrdSN b) = compare (hashStableName a) (hashStableName b)

constrain :: SimpleType -> SimpleType -> IO ()
constrain l r = go l r Set.empty >> return ()
  where
    go :: SimpleType -> SimpleType -> Set (OrdSN, OrdSN) -> IO (Set (OrdSN, OrdSN))
    go lhs rhs cache = do
        snL <- OrdSN <$> makeStableName lhs
        snR <- OrdSN <$> makeStableName rhs
        if Set.member (snL, snR) cache
            then return cache
            else do
                let nCache = Set.insert (snL, snR) cache
                case (lhs, rhs) of
                    (SPrimI32, SPrimI32) -> return nCache
                    (SPrimBool, SPrimBool) -> return nCache
                    (SFunction a1 r1, SFunction a2 r2) -> do
                        c' <- go a2 a1 nCache -- Contravariant
                        go r1 r2 c' -- Covariant
                    (SRecord fs1, SRecord fs2) ->
                        -- Width Subtyping: fs1 must have all fields that fs2 has
                        foldM
                            ( \c (n, t2) ->
                                case Map.lookup n fs1 of
                                    Just t1 -> go t1 t2 c
                                    Nothing -> error $ "Record missing field: " ++ n
                            )
                            nCache
                            (Map.toList fs2)
                    (SVariable lbs _, _) -> do
                        modifyIORef' (upperBounds lhs) (rhs :)
                        ls <- readIORef lbs
                        foldM (\c l -> go l rhs c) nCache ls
                    (_, SVariable _ ubs) -> do
                        modifyIORef' (lowerBounds rhs) (lhs :)
                        us <- readIORef ubs
                        foldM (\c u -> go lhs u c) nCache us
                    _ -> error "Type mismatch"

expand :: SimpleType -> IO Type
expand root = go root True Set.empty
  where
    go ty pol inProcess = do
        sn <- OrdSN <$> makeStableName ty
        let name = "v" ++ show (hashStableName ((\(OrdSN s) -> s) sn))
        if Set.member sn inProcess
            then return (TypeVar name)
            else do
                let nextProc = Set.insert sn inProcess
                case ty of
                    SPrimI32 -> return I32
                    SPrimBool -> return Bool
                    SFunction a r -> FunType <$> go a (not pol) nextProc <*> go r pol nextProc
                    SRecord fs -> RecordType <$> mapM (\t -> go t pol nextProc) fs
                    SVariable lbs ubs -> do
                        bounds <- readIORef (if pol then lbs else ubs)
                        if null bounds
                            then return (if pol then Bot else Top)
                            else do
                                ts <- mapM (\b -> go b pol nextProc) bounds
                                let res = foldr1 (if pol then Union else Inter) ts
                                return $ if name `occursIn` res then RecType name res else res

finalize :: TypedTermI -> IO TypedTerm
finalize ti = do
    let getTy st = simplifyType <$> expand st
    case ti of
        TIIdent s st -> TIdent s <$> getTy st
        TILitInt n -> return $ TLitInt n
        TIApp f a st -> TApp <$> finalize f <*> finalize a <*> getTy st
        TIFun n b st -> TFun n <$> finalize b <*> getTy st
        TILetIn n r b st -> TLetIn n <$> finalize r <*> finalize b <*> getTy st
        TIRecord fs st -> TRecord <$> mapM finalize fs <*> getTy st
        TISel r n st -> TSel <$> finalize r <*> pure n <*> getTy st
        TIIf c th el st -> TIf <$> finalize c <*> finalize th <*> finalize el <*> getTy st
        TIIs v ty th el st -> TIs <$> finalize v <*> pure ty <*> finalize th <*> finalize el <*> getTy st

infer :: Program -> IO TypedProgram
infer pgrm = do
    ctx <- makeBuiltins
    let defs = Set.toList pgrm

    defVars <- mapM (\(name, _) -> (name,) <$> freshVar) defs
    let extendedCtx = Map.union (Map.fromList defVars) ctx

    results <-
        mapM
            ( \(name, term) -> do
                let v = extendedCtx Map.! name
                (ti, t) <- typeTerm term extendedCtx
                constrain t v
                return (name, ti)
            )
            defs

    typedDefs <- mapM (\(name, ti) -> (name,) <$> finalize ti) results
    return $ Set.fromList typedDefs
