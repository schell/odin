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

setupNetwork :: FontData -> FontData -> System ()
setupNetwork comicFont hackFont = do
  lvl <- freshSystem $ do
    void freshPhysicsDrawingEntity
    actor1 <- fresh
    actor1 `setName` "actor1"
    actor1 `addOdinObject` OdinObject { ooVel    = (3,1)
                                      , ooRotVel = 1
                                      , ooPos    = (100,100)
                                      , ooRot    = 0
                                      , ooMass   = (1,1)
                                      , ooMu     = 0
                                      , ooHull   = rectangleHull 10 10
                                      }

    actor2 <- fresh
    actor2 `setName` "actor2"
    actor2 `addOdinObject` OdinObject { ooVel    = (-3,1)
                                      , ooRotVel = 1
                                      , ooPos    = (200,100)
                                      , ooRot    = 0
                                      , ooMass   = (1,1)
                                      , ooMu     = 0
                                      , ooHull   = rectangleHull 10 10
                                      }
  lvl `setName` "lvl"

  let button = ButtonData comicFont "Reset" 16 buttonPainter
  btn <- freshButton button (V2 200 4) $ \btn -> do
    destroyEntity lvl
    destroyEntity btn
    setupNetwork comicFont hackFont
    endScript
  btn `setName` "btn"

main :: IO ()
main = do
  comicFont <- getFont "KMKDSP__.ttf"
  hackFont  <- getFont "Hack-Regular.ttf"
  (rez,window) <- startupSDL2Backend 800 600 "Entity Sandbox" True
  putStrLn "sdl init'd"
  let step = emptySystemStep rez window
  void $ runSystem step $ do

    status <- freshStatusPrint
    --status `movePicTransform` V2 0 0
    status `setName` "status"
    setupNetwork comicFont hackFont

    io $ putStrLn "setup network"
    forever tickSystem
