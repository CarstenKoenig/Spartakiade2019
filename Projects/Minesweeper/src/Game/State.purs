module Game.State where

import Prelude hiding (div)

import Data.Array (all)
import Data.FoldableWithIndex (findWithIndex)
import Data.Maybe (Maybe(..))
import Game.Board (Board, Cell(..), State(..))
import Game.Coords (Coord)


data GameState
  = Running
  | Exploded Coord
  | Won


isRunning :: Board -> Boolean
isRunning board = case gameState board of
  Running -> true
  _       -> false


gameState :: Board -> GameState
gameState board =
  case findWithIndex revealedMine board.map of
    Just found -> Exploded found.index
    Nothing    -> if all freeRevealed board.map then Won else Running
  where 
  revealedMine _ { cell: Mine, state: Revealed } = true
  revealedMine _ _ = false
  freeRevealed { cell: Free _, state: Revealed } = true
  freeRevealed { cell: Free _, state: _        } = false
  freeRevealed _                                 = true