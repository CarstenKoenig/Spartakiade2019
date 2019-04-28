module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (className)
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)


main :: Effect Unit
main = runWidgetInDom "widget" $ counterWidget 0

counterWidget :: forall a. Int -> Widget HTML a
counterWidget count = do
  n <- D.div'
        [ D.p' [D.text ("Wert: " <> show count)]
        , map (\_ -> count+1) (D.button [ className "test", P.onClick] [D.text "Increment"])
        , D.button [P.onClick] [D.text "Decrement"] $> count-1
        , D.button [P.onClick] [D.text "reset"] $> 0
        ]

  liftEffect (log ("COUNT IS NOW: " <> show n))

  counterWidget n