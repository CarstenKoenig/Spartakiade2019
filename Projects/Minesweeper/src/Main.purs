module Main where

import Prelude hiding (div)

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div, div', div_, h1', text, p')
import Concur.React.Props (Props(..), _id, classList, className, onClick)
import Concur.React.Run (runWidgetInDom)
import Control.Plus (empty)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Game (Action, Board, Cell(..), Coord, GameState(..), State(..), cylceFlag, gameState, getCellState, initialBoard, mkSize, reveal)
import React.DOM.Props as P
import React.SyntheticEvent (SyntheticMouseEvent, preventDefault, stopPropagation)
import React.SyntheticEvent as Event


main :: Effect Unit
main = do
  board <- initialBoard (mkSize 12 20) 45
  runWidgetInDom "game" $ gameWidget board


gameWidget :: forall a . Board -> Widget HTML a
gameWidget board = do
  action <- div' rows <> overlayWidget board
  gameWidget (action board)
  where
  rows = map showRow $ Array.range 1 board.size.rows
  showRow r =
    div [ className "row" ] $ map (showCell r) $ Array.range 1 board.size.cols
  showCell r c =
    div_ [ className "cell", _id ("cell_" <> show r <> "_" <> show c) ] $
    cellWidget board { row: r, col: c }


overlayWidget :: forall a . Board -> Widget HTML a
overlayWidget board =
  case gameState board of
    Running -> empty
    Exploded minePos ->
      div 
        [ className "overlay" ] 
        [ div
          [ className "lost" ]
          [ h1' [ text "GAME OVER"] ]
        ]
    Won ->
      div 
        [ className "overlay" ] 
        [ div
          [ className "won" ]
          [ h1' [ text "YOU WIN"] ]
        ]


cellWidget :: Board -> Coord -> Widget HTML Action
cellWidget board coord =
  case getCellState board coord of
    Nothing -> 
      identity <$ empty
    Just { state: Revealed, cell: cell } ->
      identity <$ revealedCellWidget cell
    Just { state, cell } ->
      button 
        [ reveal coord <$ onClick
        , cylceFlag coord <$ onRightClick 
        , classList 
          [ Just (stateClass state)
          , if cell == Mine then Just "vorsicht" else Nothing
          ]
        ] 
        [ ]
  where
  stateClass Hidden = "hidden"
  stateClass Flagged = "flagged"
  stateClass Marked = "marked"
  stateClass Revealed = "revealed"


revealedCellWidget :: forall a . Cell -> Widget HTML a
revealedCellWidget = case _ of
  Mine   -> div [ className "revealed mine" ] [ ]
  Free 0 -> div [ className "revealed empty" ] [ ]
  Free n -> div [ className "revealed empty" ] [ p' [ text (show n) ] ]


onRightClick :: Props SyntheticMouseEvent
onRightClick = Handler handle
  where
  handle useEv = P.onContextMenu (\ev -> preventDefault ev *> useEv ev)