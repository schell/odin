--{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
import Gelatin.SDL2
import Control.Monad (void, forever, forM_)
import Control.Concurrent (threadDelay)
import Control.Varying
import Data.Monoid ((<>))
import Data.Functor.Identity
import Text.Show.Pretty
import System.FilePath
import System.Directory
import System.Exit

import Data.Tiled
import Data.Tiled.Utils
import Odin.Common
import Odin.Component
import Odin.Physics
import Odin.System
import Odin.Styles
import Odin.Scripts.Animation.Pic
import Odin.Scripts.ArrowControl
import Odin.Scripts.Button
import Odin.Scripts.DrawPhysics
import Odin.Scripts.Status

-- | Load our standard fonts.
getFont :: String -> IO FontData
getFont name = do
    -- Get our fonts
    assets <- (</> "assets") <$> getCurrentDirectory
    -- Load our header font
    let font = assets </> "fonts" </> name
    loadFont font >>= \case
      Left err -> do print err
                     exitFailure
      Right fnt -> return fnt

setupNetwork :: FontData -> System ()
setupNetwork font = do
  net <- fresh

  lvl <- freshSystem $ do
    void freshPhysicsDrawingEntity
    actor <- fresh
    actor `addOdinObject` OdinObject { ooVel    = (3,1)
                                     , ooRotVel = 1
                                     , ooPos    = (0,0)
                                     , ooRot    = 0
                                     , ooMass   = (1,1)
                                     , ooMu     = 0
                                     , ooHull   = rectangleHull 10 10
                                     }

    status <- freshStatusBar font
    status `movePicTransform` V2 0 20

  let button = ButtonData font "Reset" 16 buttonPainter
  void $ freshButton button 100 $ \btn -> do
    destroyEntity lvl
    destroyEntity net
    destroyEntity btn
    setupNetwork font
    endScript


main :: IO ()
main = do
  font <- getFont "KMKDSP__.ttf"
  (rez,window) <- startupSDL2Backend 800 600 "Entity Sandbox" True
  putStrLn "sdl init'd"
  let step = emptySystemStep rez window
  void $ runSystem step $ do
    setupNetwork font
    io $ putStrLn "setup network"
    forever $ do
      tickSystem
      io $ threadDelay 1
