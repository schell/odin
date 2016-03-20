module Main where

import Odin.Common
import Odin.Infrastructure
import Gelatin.GL
import Control.Varying
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
gameOver = pure (OdinEnd GameOverScreen) `untilEvent_` anyKeydown >>= step

mainGame :: Sequence ()
mainGame = do
  let f wsize = OdinRun $ Board Interface mempty wsize
  b <- (f <$> windowSize) `untilEvent_` anyKeydown
  step b

networkSpline :: Sequence ()
networkSpline = do
  mainGame
  let f = OdinPic $ \cfg ->
        move (V2 0 64) $ withLetters $ filled (Name 0) (ocFancyFont cfg) 128 64
          "Resetting..." $ solid red
  pure f `_untilEvent_` (time ~> after 1)
  --titleScreen
  --gameOver
  networkSpline

main :: IO ()
main = run $ outputStream networkSpline (OdinPic $ const blank)

