module Algorithm.Closure where

import Prelude hiding (div)

import Data.Set (Set)
import Data.Set as Set


closure :: forall a . Ord a => (a -> Set a) -> a -> Set a
closure neighbors start = Set.empty