module Main where

import Prelude hiding (div)

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text)
import Concur.React.Props (Props(..))
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import Effect.Class (liftEffect)
import Game (Action, Board, Coord, cylceFlag, initialBoard, mkSize, reveal)
import React.DOM.Props as P
import React.SyntheticEvent (SyntheticMouseEvent, preventDefault, stopPropagation)
import React.SyntheticEvent as Event


main :: Effect Unit
main = do
  board <- initialBoard (mkSize 12 20) 45
  runWidgetInDom "game" $ gameWidget board


gameWidget :: forall a . Board -> Widget HTML a
gameWidget board = do
  text "Minesweeper"

onRightClick :: Props SyntheticMouseEvent
onRightClick = Handler handle
  where
  handle useEv = P.onContextMenu (\ev -> preventDefault ev *> useEv ev)


handleClick :: Coord -> SyntheticMouseEvent -> Widget HTML Action
handleClick coord mouseEv = do
  buttonNr <- liftEffect do
    preventDefault mouseEv
    stopPropagation mouseEv
    nr <- Event.button mouseEv 
    pure nr
  case buttonNr of
    0.0 -> pure $ reveal coord
    _   -> pure $ cylceFlag coord