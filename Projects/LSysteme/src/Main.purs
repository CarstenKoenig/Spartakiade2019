module Main where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (concatMap, uncons)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Graphics.Canvas (Context2D, closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = void $ unsafePartial $ do
  Just canvas <- getCanvasElementById "output"
  ctx <- getContext2D canvas

  setFillStyle "#0000FF" ctx
  fillPath ctx $ rect ctx
    { x: 250.0
    , y: 250.0
    , w: 100.0
    , h: 100.0
    }  