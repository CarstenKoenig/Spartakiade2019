module Game.State where

import Prelude hiding (div)

import Game.Board (Board)
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
gameState board = Running