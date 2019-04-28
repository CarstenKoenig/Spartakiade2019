module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as H
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Plus (empty)
import Data.Array (fold, (..))
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Game (Board, BoardSize, CellContent(..), CellState(..), Coord, getCell, randomBoard, revealCell)

gameSize :: BoardSize
gameSize = { rowCount: 7, colCount: 10 }

main :: Effect Unit
main = do
  board <- randomBoard gameSize 10
  runWidgetInDom "game" (gameWidget board)

gameWidget :: forall a. Board -> Widget HTML a
gameWidget = go
  where
  go board = do 
    action <- H.div
      [ P.className "container" ]
      [ renderBoard board
      ]
    
    -- verarbeite Aktion ...
    let 
      newBoard =
        case action of
          Reveal coord -> revealCell coord board

    go newBoard

data Action
  = Reveal Coord

renderBoard :: Board -> Widget HTML Action
renderBoard board =
  fold (map 
    (rendereZeile board board.size.colCount) 
    (1..board.size.rowCount))

rendereZeile :: Board -> Int -> Int -> Widget HTML Action
rendereZeile board colCount row =
  H.div
    [ P.className "row no-gutters" ]
    (map (\col -> rendereZelle board { row, col }) (1..colCount)
    )


rendereZelle :: Board -> Coord -> Widget HTML Action
rendereZelle board coord =
  case getCell board coord of
    Just { state: Hidden, content: _ } ->
      H.div
        [ P.className "col-auto cell" 
        ]
        [ H.button 
          [ P.onClick $> Reveal coord 
          , P.className "cell"
          , P.style { backgroundColor: bgColor }
          ] 
          []
        ]
    Just { state: Revealed, content: Mine } ->
      H.div
        [ P.className "col-auto cell" 
        ]
        [ H.text "*" ]
    Just { state: Revealed, content: Empty 0 } ->
      H.div
        [ P.className "col-auto cell" 
        ]
        [ H.div [ P.className "cell"] [] ]
    Just { state: Revealed, content: Empty n } ->
      H.div
        [ P.className "col-auto cell" 
        ]
        [ H.text (show n) ]
    _ -> empty
  where
  bgColor = if isMine then "red" else "green"
  cell = getCell board coord
  isMine = 
    maybe false 
      (\c -> c.content == Mine)
      (getCell board coord)

