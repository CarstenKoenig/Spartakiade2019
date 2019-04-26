module Game.Actions where

import Prelude hiding (div)
import Game.Board (Board)
import Game.Coords (Coord)



type Action = Board -> Board


reveal :: Coord -> Action
reveal coord board = board


cylceFlag :: Coord -> Action
cylceFlag coord = identity