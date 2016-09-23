{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Gelatin.SDL2 hiding (move)
import           SDL
import           Gelatin.FreeType2
import           Odin.GUI
import           Odin.Core
import           Control.Lens
import           Halive.Utils
import qualified Demos.Physics as Physics
import qualified Demos.Animation as Animation

runFrame :: MonadIO m => UpdateT m a -> StateT Frame m a
runFrame f = do
  io $ glClearColor 0.5 0.5 0.5 1
  use rez >>= io . clearFrame
  tickTime
  tickUIPrepare
  e <- runEventT f
  use window >>= io . updateWindowSDL2
  case e of
    Left g   -> runFrame g
    Right a  -> return a

data DemoSelection = DemoPhysics | DemoAnimation

demoSelectTask :: MonadIO m => UpdateT m ()
demoSelectTask = withDefaultStatusBar white $ \status ->
  withDefaultButton "Physics" $ \physBtn -> do
    V2 physw physh <- sizeOfButton physBtn
    withDefaultButton "Animation" $ \aniBtn -> do
      selection <- slot DemoAnimation
      physDemo  <- Physics.allocDemo
      aniDemo   <- Animation.allocDemo
      fix $ \continue -> do
        V2 _ ch <- (io . SDL.get . windowSize) =<< use window
        let wh = fromIntegral ch
        renderStatusBar status [move 0 $ wh - 2]
        renderButton physBtn [] >>= \case
          ButtonStateClicked -> do
            reslot selection DemoPhysics
            Physics.resumeDemo physDemo
          _ -> return ()
        renderButton aniBtn [move (physw + 4) 0] >>= \case
          ButtonStateClicked -> do
            reslot selection DemoAnimation
            Physics.pauseDemo physDemo
          _ -> return ()
        let rs = [move 0 $ physh + 4]
        unslot selection >>= \case
          DemoPhysics   -> Physics.renderDemo physDemo rs
          DemoAnimation -> Animation.renderDemo aniDemo rs
        next continue

main :: IO ()
main = do
  (rz,win)  <- reacquire 0 $ startupSDL2Backend 800 600 "Entity Sandbox" True
  t         <- newTime
  let firstFrame = Frame { _frameTime   = t
                         , _frameEvents = []
                         , _frameNextK  = 0
                         , _frameWindow = win
                         , _frameRez    = rz
                         , _frameFonts  = mempty
                         }
  void $ runStateT (runFrame demoSelectTask) firstFrame
