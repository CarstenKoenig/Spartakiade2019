module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as H
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Data.Array (fold, (..))
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Game (Board, BoardSize, CellContent(..), Coord, getCell, randomBoard)

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
    case action of
      Reveal coord -> liftEffect (logShow coord)

    go board

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
  H.div
    [ P.className "col-auto" 
    ]
    [ H.button 
      [ P.onClick $> Reveal coord 
      , P.style { backgroundColor: bgColor }
      ] 
      []
    ]
  where
  bgColor = if isMine then "red" else "green"
  isMine = 
    maybe false 
      (\cell -> cell.content == Mine)
      (getCell board coord)

