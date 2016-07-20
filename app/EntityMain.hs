--{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
import Gelatin.SDL2
import Control.Monad (void, forever, forM_)
import Control.Concurrent (threadDelay)
import Control.Varying
import Data.Monoid ((<>))
import Data.Functor.Identity
import System.FilePath
import System.Directory
import System.Exit

import Data.Tiled
import Data.Tiled.Utils
import Odin.Core
import Odin.Styles
import Odin.Scripts.Animation.Pic
import Odin.Scripts.ArrowControl
import Odin.Scripts.Button
import Odin.Scripts.DrawPhysics
import Odin.Scripts.Status

import Halive.Utils

-- | Load our standard fonts.
getFont :: String -> IO FontData
getFont fontname = do
    -- Get our fonts
    assets <- (</> "assets") <$> getCurrentDirectory
    -- Load our header font
    let font = assets </> "fonts" </> fontname
    loadFont font >>= \case
      Left err -> do print err
                     exitFailure
      Right fnt -> return fnt

setupNetwork :: FontData -> FontData -> System ()
setupNetwork comicFont hackFont = do
  lvl <- freshSystem $ do
    void freshPhysicsDrawingEntity
    forM_ [(x,y) | y <- [0..5], x <- [0..5]] $ \(x,y) ->
      fresh ## name ("actor" ++ show x ++ show y)
            #. body Body{ bVel    = (8,1)
                        , bRotVel = 1
                        , bPos    = (100 + x*11,100 + y*11)
                        , bRot    = 0
                        , bMass   = (1,1)
                        , bMu     = 0
                        , bHull   = rectangleHull 10 10
                        }

    fresh ## name "actor2"
          #. body Body{ bVel    = (-3,2)
                      , bRotVel = 1
                      , bPos    = (400,100)
                      , bRot    = 0
                      , bMass   = (10,10)
                      , bMu     = 0
                      , bHull   = rectangleHull 20 20
                      }

    fresh ## name "wall"
          #. body Body{ bVel    = (0,0)
                      , bRotVel = 0
                      , bPos    = (700, 300)
                      , bRot    = 0
                      , bMass   = (0,0)
                      , bMu     = 0
                      , bHull   = rectangleHull 10 600
                      }
  setName lvl "lvl"

  let button = ButtonData comicFont "Reset" 16 buttonPainter
  btnMail <- mailbox
  btn     <- freshButton button 4 btnMail
               ## name "btn"
  recv btnMail $ \() -> do
    modify $ \t -> t{ timeDelta = 0, timeLeft = 0 }
    destroyEntity lvl
    destroyEntity btn
    setupNetwork comicFont hackFont

main :: IO ()
main = do
  comicFont <- getFont "KMKDSP__.ttf"
  hackFont  <- getFont "Hack-Regular.ttf"
  (rez,window) <- reacquire 0 $ startupSDL2Backend 800 600 "Entity Sandbox" True
  runSystem (emptySystemStep rez window) $ do
    status <- freshStatusPrint
    status `setName` "status"
    setupNetwork comicFont hackFont
    forever tickSystem
