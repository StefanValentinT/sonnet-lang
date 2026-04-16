module TyperHelpers (occursIn) where

import Syntax

occursIn n (TypeVar v) = n == v
occursIn n (Union a b) = occursIn n a || occursIn n b
occursIn n (Inter a b) = occursIn n a || occursIn n b
occursIn n (FunType a b) = occursIn n a || occursIn n b
occursIn n (RecordType fs) = any (occursIn n) fs
occursIn n (RecType v t) = if n == v then False else occursIn n t
occursIn _ _ = False
