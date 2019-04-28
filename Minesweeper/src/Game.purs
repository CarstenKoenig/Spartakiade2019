module Game
  ( Board, BoardSize
  , Cell, CellContent(..), CellState(..)
  , Coord
  , emptyBoard
  , insertMine
  , getCell
  , revealCell
  , randomBoard
  ) where

import Prelude

import Algorithm.Closure (closure)
import Control.MonadZero (guard)
import Data.Array (cons, (..))
import Data.Foldable (foldl, length)
import Data.List (filter)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomInt)

type Board = 
  { size :: BoardSize
  , map  :: Map Coord Cell
  }

type Cell =
  { content :: CellContent
  , state   :: CellState
  }

data CellContent
  = Mine
  | Empty Int

derive instance eqCellContent :: Eq CellContent

data CellState
  = Hidden
  | Flagged
  | Marked
  | Revealed

derive instance eqCellState :: Eq CellState

type BoardSize =
  { rowCount :: Int
  , colCount :: Int
  }

type Coord = { row :: Int, col :: Int }

emptyBoard :: BoardSize -> Board
emptyBoard boardSize =
  { size: boardSize
  , map: Map.fromFoldable emptyCells
  }
  where
  emptyCells = do
    row <- 1 .. boardSize.rowCount
    col <- 1 .. boardSize.colCount
    let coord = { row, col }
    let cell =
          { content: Empty 0
          , state: Hidden
          }
    pure (Tuple coord cell)

getCell :: Board -> Coord -> Maybe Cell
getCell board coord =
  Map.lookup coord board.map

revealCell :: Coord -> Board -> Board
revealCell coord board =
  foldl
    (flip setRevealed)
    board
    (closure neighbors coord)
  where
  neighbors coord' =
    case getCell board coord' of
      Just { content: Empty 0 } -> Set.fromFoldable $ do
        row <- (coord'.row-1) .. (coord'.row+1)
        col <- (coord'.col-1) .. (coord'.col+1)
        guard (row /= coord'.row || col /= coord'.col)
        pure { row, col }
      _ -> Set.empty

setRevealed :: Coord -> Board -> Board
setRevealed coord board =
  updateCell update coord board
  where
  update cell =
    cell { state = Revealed }

insertMine :: Coord -> Board -> Board
insertMine coord board =
  if alreadyMineAtPos
    then board
    else 
      foldl 
        (\aktBoard aktion -> aktion aktBoard)
        board 
        -- Array (Board -> Board)
        (setMine coord `cons` map incrMineCounter neighbors)
  where
  alreadyMineAtPos =
    maybe true (\cell -> cell.content == Mine)
      (getCell board coord)
  neighbors = do
    row <- (coord.row-1) .. (coord.row+1)
    col <- (coord.col-1) .. (coord.col+1)
    guard (row /= coord.row || col /= coord.col)
    pure { row, col}

setMine :: Coord -> Board -> Board
setMine coord board =
  updateCell update coord board
  where
  update cell =
    cell { content = Mine }

incrMineCounter :: Coord -> Board -> Board
incrMineCounter coord board =
  updateCell update coord board
  where
  update cell =
    let 
      newContent =
        case cell.content of
          Mine -> Mine
          Empty n -> Empty (n+1)
    in cell { content = newContent }

updateCell :: (Cell -> Cell) -> Coord -> Board -> Board
updateCell upd coord board = board { map = newMap }
  where
  newMap =
    Map.update (\oldCell -> Just (upd oldCell)) 
               coord board.map

randomBoard :: BoardSize -> Int -> Effect Board
randomBoard boardSize nrMines = go (emptyBoard boardSize)
  where
  go board 
    | minesOnBoard board == nrMines = pure board
    | otherwise = do
      coord <- randomCoord boardSize
      go (insertMine coord board)
  minesOnBoard board = length $
    filter 
      (\cell -> cell.content == Mine) 
      (Map.values board.map)

randomCoord :: BoardSize -> Effect Coord
randomCoord boardSize = do
  row <- randomInt 1 boardSize.rowCount
  col <- randomInt 1 boardSize.colCount
  pure { row, col }