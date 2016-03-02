module Main where

import Odin.Common
import Odin.Infrastructure
import Gelatin.GL
import Control.Varying
import Control.Monad (void)
import SDL.Event hiding (Event)

anyKeyup :: Signal (Event ())
anyKeyup = var f ~> onJust
    where f (InputKey Released _ _) = Just ()
          f _ = Nothing

anyKeydown :: Signal (Event ())
anyKeydown = var f ~> onJust
    where f (InputKey Pressed _ _) = Just ()
          f _ = Nothing

percentageOfSeconds :: Float -> Signal Float
percentageOfSeconds s = (/ s) <$> (time ~> accumulate (+) 0)

titleScreen :: Sequence ()
titleScreen = pure (OdinStart StartScreenWait) `untilEvent_` anyKeydown >>= step

gameOver :: Sequence ()
gameOver = pure (OdinGameOver GameOverScreen) `untilEvent_` anyKeydown >>= step

networkSpline :: Sequence ()
networkSpline = do
  titleScreen
  gameOver
  networkSpline

main :: IO ()
main = run $ outputStream (OdinPic blank) networkSpline

