module SimpleType (SimpleType (..), freshVar) where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data SimpleType
    = SPrimI32
    | SPrimBool
    | SVariable
        { lowerBounds :: IORef [SimpleType]
        , upperBounds :: IORef [SimpleType]
        }
    | SFunction SimpleType SimpleType
    | SRecord (Map String SimpleType)

freshVar :: IO SimpleType
freshVar = do
    lowers <- newIORef []
    uppers <- newIORef []
    return $ SVariable lowers uppers
