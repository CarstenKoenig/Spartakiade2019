module Algorithm.Closure where

import Prelude hiding (div)

import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set


closure :: forall a . Ord a => (a -> Set a) -> a -> Set a
closure neighbors start = 
  go Set.empty (Set.singleton start)
  where
  go visited lookAt =
    case Set.findMin lookAt of
      Nothing -> visited
      Just nextCoord ->
        let 
          visited' = Set.insert nextCoord visited
          lookAt'  = (lookAt `Set.union` neighbors nextCoord) `Set.difference` visited'
        in go visited' lookAt'