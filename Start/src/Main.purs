module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concatMap, filter, null, (..))
import Data.Array.Partial (tail)
import Data.Foldable (product)
import Effect (Effect)
import Effect.Console (log)
import Math (pi, sqrt)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = do
  log "Hello sailor!"


diagonal :: Number -> (Number -> Number)
diagonal w h = sqrt (w*w + h*h)


circleArea :: Number -> Number
circleArea r = r * r * pi

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length (unsafePartial (tail arr))


-- isEven 0 = true
-- isEven 1 = false
-- isEven 2 = true
-- ...
isEven :: Int -> Boolean
isEven 0 = true
isEven n = isOdd (n-1)

isOdd :: Int -> Boolean
isOdd 0 = false
isOdd n = isEven (n-1)


-- suche paarweise Faktoren einer Zahl
-- 10 - 1*10, 2*5

factors :: Int -> Array (Array Int)
factors n =
  filter (\pair -> product pair == n) (pairs n)

-- (>>=) = flip concatMap

pairs :: Int -> Array (Array Int)
pairs n =
  concatMap
    (\i -> map (\j -> [i,j]) (i .. n) )
    (1 .. n)

pairs' :: Int -> Array (Array Int)
pairs' n =
    (1 .. n) >>=
      (\i -> (i..n) >>= 
        (\j -> pure [i,j]))

pairs'' :: Int -> Array (Array Int)
pairs'' n = do
  i <- 1 .. n
  j <- i .. n
  pure [i,j]

factors' :: Int -> Array (Array Int)
factors' n = filter (\pair -> product pair == n) (do
  i <- 1 .. n
  j <- i .. n
  pure [i,j])

factors'' :: Int -> Array (Array Int)
factors'' n = do
  i <- 1 .. n
  j <- i .. n
  guard (i*j == n)
  pure [i,j]

-- [3,4,5] ok => 3*3 + 4*4 = 5*5
-- [3,4,6] nicht ok => 6*6 = 36 != 3*3 + 4*4

triple :: Int -> Array (Array Int)
triple n = do
  i <- 1 .. n
  j <- i .. n
  k <- j .. n
  guard (i*i+j*j == k*k)
  pure [i,j,k]

triple' :: Int -> Array (Array Int)
triple' n =
  (1..n) >>= (\i ->
    (i..n) >>= (\j -> 
      (j..n) >>= (\k -> 
        guard (i*i+j*j == k*k) >>= (\_ ->
          pure [i,j,k] 
          ) 
        )
      )
   )

data Option a
  = None
  | Some a

mapOption :: forall a b. (a -> b) -> Option a -> Option b
mapOption f opt =
  case opt of
    None -> None
    Some a -> Some (f a)

newtype Name = Name String

derive instance eqName :: Eq Name

carsten :: Name
carsten = Name "Carsten"

data Name2 = Name2 String

carsten2 :: Name2
carsten2 = Name2 "Carsten"

class Zeige a where
  zeige :: a -> String

instance zeigeString :: Zeige String where
  zeige s = "String: " <> s

else instance zeigeInt :: Zeige Int where
  zeige n = "Int: " <> show n

else instance zeigeShow :: Show a => Zeige a where
  zeige x = show x

zeigeLaut 
  :: forall a b. 
  Zeige a => 
  Zeige b => 
  a -> b -> String
zeigeLaut x y = zeige x <> zeige y <> "!"