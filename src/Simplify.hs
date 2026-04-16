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
    Union (RecordType fs1) (RecordType fs2) ->
        if Map.keysSet fs1 == Map.keysSet fs2
            then RecordType $ Map.intersectionWith (\t1 t2 -> compact (Union t1 t2)) fs1 fs2
            else Union (RecordType fs1) (RecordType fs2)
    Inter (RecordType fs1) (RecordType fs2) ->
        let merged =
                Map.mergeWithKey
                    (\_ t1 t2 -> Just $ compact (Inter t1 t2))
                    (Map.map (`Inter` Top))
                    (Map.map (Inter Top))
                    fs1
                    fs2
         in RecordType merged
    Union t1 t2 ->
        let a = compact t1; b = compact t2
         in if a == b
                then a
                else
                    if a == Bot
                        then b
                        else
                            if b == Bot
                                then a
                                else
                                    if a == Top || b == Top
                                        then Top
                                        else case (a, b) of
                                            (RecordType _, RecordType _) -> compact (Union a b)
                                            _ -> Union a b
    Inter t1 t2 ->
        let a = compact t1; b = compact t2
         in if a == b
                then a
                else
                    if a == Top
                        then b
                        else
                            if b == Top
                                then a
                                else if a == Bot || b == Bot then Bot else Inter a b
    FunType arg res -> FunType (compact arg) (compact res)
    RecordType fs -> RecordType (Map.map compact fs)
    RecType n t -> let t' = compact t in if n `occursIn` t' then RecType n t' else t'
    other -> other

lookupField k fs = maybe Bot id (lookup k fs)
lookupFieldDef d k fs = maybe d id (lookup k fs)
