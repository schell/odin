{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Demos.Physics1 ( demo ) where

import Control.Lens hiding (to)
import qualified Data.Set as S
import Linear

import Gelatin
import Gelatin.FreeType2
import Odin.Core
import Odin.GUI
import Odin.Scripts.Status
import Odin.GUI.TextInput.Internal

makeDemoActors :: System [Entity]
makeDemoActors = do
  let actorPic w h = do
        setStroke [StrokeWidth 3, StrokeFeather 1]
        setGeometry $ do
          let r1 = red `withAlpha` 0.1
              r3 = red `withAlpha` 0.3
              r5 = red `withAlpha` 0.5
          fan $ mapVertices (, r3) $
            rectangle (V2 (-w/2) (-h/2)) (V2 (w/2) (h/2))
          line $ do
            to (0, r1)
            to (V2 (w/2) 0, r5)
            to (V2 (w/2) (h/2), r5)
            to (V2 (-w/2) (h/2), r5)
            to (V2 (-w/2) (-h/2), r5)
            to (V2 (w/2) (-h/2), r5)
            to (V2 (w/2) 0, r5)
  actors <- forM [(x,y) | y <- [0..5], x <- [0..5]] $ \(x,y) -> do
    let tx = 100 + x*11
        ty = 100 + y*11
    fresh ## name ("actor" ++ show x ++ show y)
          ## pos 0
          ## colorPic (actorPic 10 10)
          ## body Body{ bVel    = (5,5)
                      , bRotVel = 0
                      , bPos    = (tx, ty)
                      , bRot    = 0
                      , bMass   = (1,1)
                      , bMu     = 0
                      , bHull   = rectangleHull 10 10
                      }

  biggy <- fresh ## name "biggy"
                 ## pos 0
                 ## colorPic (actorPic 10 30)
                 ## body Body{ bVel    = (0,0)
                             , bRotVel = 1
                             , bPos    = (180,180)
                             , bRot    = 0
                             , bMass   = (0,0)
                             , bMu     = 0
                             , bHull   = rectangleHull 10 30
                             }

  rwall <- fresh ## name "rightwall"
                 ## pos 0
                 ## colorPic (actorPic 10 289)
                 ## body Body{ bVel    = (0,0)
                             , bRotVel = 0
                             , bPos    = (300, 150)
                             , bRot    = 0
                             , bMass   = (0,0)
                             , bMu     = 0
                             , bHull   = rectangleHull 10 289
                             }
  bwall <- fresh ## name "bottomwall"
                 ## pos 0
                 ## colorPic (actorPic 289 10)
                 ## body Body{ bVel    = (0,0)
                             , bRotVel = 0
                             , bPos    = (150, 300)
                             , bRot    = 0
                             , bMass   = (0,0)
                             , bMu     = 0
                             , bHull   = rectangleHull 289 10
                             }


  return $ actors ++ [biggy, rwall, bwall]

loop :: FilePath -> System ()
loop font = do
  -- Alloc a text atlas to use for our UI
  let sz = PixelSize 16 16
  Just atlas <- allocAtlas font sz asciiChars
  -- Create a status bar to tell us what's up
  status  <- freshStatusBar atlas ## name "status"
                                  ## pos (V2 400 16)
  -- Create all of our demo actors
  actors  <- makeDemoActors
  -- Keep a ref to the pristine physics scene
  scene0  <- use scene

  -- Make a button to reset the demo
  bview   <- allocButtonView atlas "Reset" sz buttonPainter (const $ return ())
  btn     <- freshButton 4 bview ## name "btn"
  recv (btnMailbox bview) $ \btnState ->
    -- When the button sends a 'clicked' message we reset the demo by simply
    -- putting the original scene back in place
    when (btnState == ButtonStateClicked) (scene .= scene0)

  tview   <- allocTextInputView atlas "text..." sz textInputPainter $
    io . print
  void $ freshTextInput (V2 4 400) tview

demo :: FilePath -> System ()
demo = loop
