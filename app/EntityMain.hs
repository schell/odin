{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Gelatin.SDL2
import           Gelatin.FreeType2
import           Odin.GUI
import           Odin.Core
import           Control.Lens
import           Halive.Utils
import qualified Demos.Physics1 as Physics1
--import qualified Demos.MapCreator as MapCreator

runFrame :: MonadIO m => UpdateT m a -> StateT Frame m a
runFrame f = do
  use rez >>= io . clearFrame
  tickTime
  tickEvents
  tickPhysics
  e <- runEventT f
  use window >>= io . updateWindowSDL2
  case e of
    Left g   -> runFrame g
    Right a  -> return a

main :: IO ()
main = do
  --forkServer "localhost" 8000
  (rz,win)  <- reacquire 0 $ startupSDL2Backend 800 600 "Entity Sandbox" True
  t         <- newTime
  let firstFrame = Frame { _frameTime   = t
                         , _frameEvents = []
                         , _frameNextK  = 0
                         , _frameWindow = win
                         , _frameRez    = rz
                         , _frameScene  = emptyScene
                         , _frameFonts  = mempty
                         }
  void $ runStateT (runFrame Physics1.demo) firstFrame
