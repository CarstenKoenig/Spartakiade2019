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

  let kochCurve = makeKoch ctx

  setFillStyle ctx "#00AAFF"
  setStrokeStyle ctx "#000088"
  fillPath ctx kochCurve
  strokePath ctx kochCurve
  where

  makeKoch ctx = do
    moveTo ctx initialState.x initialState.y
    _ <- lsystem initial productions (interpret ctx) 5 initialState
    closePath ctx


--------------------------------------------------------------------------------
-- L-System für eine Koch-Kurve

-- | L = rotiere 45° links, R = rotiere 45° rechts, F = gerader Strich
data Alphabet = L | R | F

type Sentence = Array Alphabet

-- | initialer Satz = Rechteck
initial :: Sentence
initial = [F, R, R, F, R, R, F, R, R]

-- | ersetze jeden Strich _ mit einem _/\_
productions :: Alphabet -> Sentence
productions L = [L]
productions R = [R]
productions F = [F, L, F, R, R, F, L, F]

--------------------------------------------------------------------------------
-- Interpretation auf dem Canvas über Position und Winkel gegen X-Achse

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

initialState :: State
initialState = { x: 120.0, y: 200.0, theta: 0.0 }

interpret :: Context2D -> State -> Alphabet -> Effect State
interpret _ state L = pure $ state { theta = state.theta - Math.pi / 3.0 }
interpret _ state R = pure $ state { theta = state.theta + Math.pi / 3.0 }
interpret ctx state F = do
  let x = state.x + Math.cos state.theta * 1.9
      y = state.y + Math.sin state.theta * 1.9
  lineTo ctx x y
  pure { x, y, theta: state.theta }


-- | L-System
-- | ersetzt `depth` mal jedes `a` in `init` mit Hilfe von `prod`
-- | sind aller Iterationen durchgeführt benutze *falte* mit `iter` über `state`
lsystem :: forall a s 
         . Array a                -- initialer Sentence
        -> (a -> Array a)         -- production function
        -> (s -> a ->  Effect s)
        -> Int
        -> s
        -> Effect s
lsystem init prod inter depth state = 
  let
    go s 0 = foldM' inter state s
    go s n = go (concatMap prod s) (n - 1)
  in go init depth

-- | leider ist das foldM, dass man direkt findet nicht Stack-safe
-- | die vielen Effekte würden zu einem Stack-Overflow führen
-- | glücklicherweise gibt es tailRecM
foldM' :: forall s a . (s -> a -> Effect s) -> s -> Array a -> Effect s
foldM' f s0 as =
  tailRecM go (Tuple s0 as)
  where
  go (Tuple s arr) = 
    case uncons arr of
      Just { head: x, tail: xs } -> do
        s' <- f s x
        pure (Loop (Tuple s' xs))
      Nothing -> pure (Done s)
