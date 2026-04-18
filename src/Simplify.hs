module Simplify (simplifyType) where

import Data.List (nub, sort)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Syntax
import TyperHelpers

simplifyType :: Type -> Type
simplifyType ty =
    let pols = gatherPolarities ty True Map.empty
        eliminated = eliminateDeadVars pols ty
     in compact eliminated

type Polarities = Map String (Bool, Bool)

gatherPolarities :: Type -> Bool -> Polarities -> Polarities
gatherPolarities ty pol acc = case ty of
    TypeVar v ->
        let (p, n) = Map.findWithDefault (False, False) v acc
         in Map.insert v (p || pol, n || not pol) acc
    FunType arg res -> gatherPolarities res pol (gatherPolarities arg (not pol) acc)
    RecordType fs -> Map.foldr (\t a -> gatherPolarities t pol a) acc fs
    Union t1 t2 -> gatherPolarities t2 pol (gatherPolarities t1 pol acc)
    Inter t1 t2 -> gatherPolarities t2 pol (gatherPolarities t1 pol acc)
    RecType _ t -> gatherPolarities t pol acc
    _ -> acc

eliminateDeadVars :: Polarities -> Type -> Type
eliminateDeadVars pols ty = case ty of
    TypeVar v -> case Map.lookup v pols of
        Just (True, False) -> Bot
        Just (False, True) -> Top
        _ -> TypeVar v
    FunType a r -> FunType (eliminateDeadVars pols a) (eliminateDeadVars pols r)
    RecordType fs -> RecordType (Map.map (eliminateDeadVars pols) fs)
    Union a b -> Union (eliminateDeadVars pols a) (eliminateDeadVars pols b)
    Inter a b -> Inter (eliminateDeadVars pols a) (eliminateDeadVars pols b)
    RecType n t -> RecType n (eliminateDeadVars pols t)
    other -> other

compact :: Type -> Type
compact ty = case ty of
    RecordType fs -> RecordType (Map.map compact fs)
    FunType arg res -> FunType (compact arg) (compact res)
    Inter t1 t2 ->
        let a = compact t1; b = compact t2
         in case (a, b) of
                _ | a == b -> a
                (Top, _) -> b
                (_, Top) -> a
                (Bot, _) -> Bot
                (_, Bot) -> Bot
                (RecordType fs1, RecordType fs2) ->
                    RecordType $ Map.unionWith (\v1 v2 -> compact (Inter v1 v2)) fs1 fs2
                _ -> Inter a b
    Union t1 t2 ->
        let a = compact t1; b = compact t2
         in case (a, b) of
                _ | a == b -> a
                (Top, _) -> Top
                (_, Top) -> Top
                (Bot, _) -> b
                (_, Bot) -> a
                (RecordType fs1, RecordType fs2)
                    | Map.keysSet fs1 == Map.keysSet fs2 ->
                        RecordType $ Map.intersectionWith (\v1 v2 -> compact (Union v1 v2)) fs1 fs2
                _ -> Union a b
    RecType n t -> let t' = compact t in if n `occursIn` t' then RecType n t' else t'
    other -> other

lookupField k fs = maybe Bot id (lookup k fs)
lookupFieldDef d k fs = maybe d id (lookup k fs)
