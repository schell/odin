{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Demos.Physics1 ( demo ) where

import Control.Lens
import qualified Data.Set as S

import Gelatin.Fruity
import Odin.Core
import Odin.GUI
import Odin.Scripts.DrawPhysics
import Odin.Scripts.Status

makeDemoActors :: System [Entity]
makeDemoActors = do
  actors <- forM [(x,y) | y <- [0..5], x <- [0..5]] $ \(x,y) ->
    fresh ## name ("actor" ++ show x ++ show y)
          ## body Body{ bVel    = (5,5)
                      , bRotVel = 1
                      , bPos    = (100 + x*11,100 + y*11)
                      , bRot    = 0
                      , bMass   = (1,1)
                      , bMu     = 0
                      , bHull   = rectangleHull 10 10
                      }

  biggy <- fresh ## name "biggy"
                 ## body Body{ bVel    = (0,0)
                             , bRotVel = 1
                             , bPos    = (180,180)
                             , bRot    = 0
                             , bMass   = (0,0)
                             , bMu     = 0
                             , bHull   = rectangleHull 10 30
                             }

  rwall <- fresh ## name "rightwall"
                 ## body Body{ bVel    = (0,0)
                             , bRotVel = 0
                             , bPos    = (300, 150)
                             , bRot    = 0
                             , bMass   = (0,0)
                             , bMu     = 0
                             , bHull   = rectangleHull 10 289
                             }
  bwall <- fresh ## name "bottomwall"
                 ## body Body{ bVel    = (0,0)
                             , bRotVel = 0
                             , bPos    = (150, 300)
                             , bRot    = 0
                             , bMass   = (0,0)
                             , bMu     = 0
                             , bHull   = rectangleHull 289 10
                             }


  return $ actors ++ [biggy, rwall, bwall]

loop :: Font -> System ()
loop font = do
  -- Create a status bar to tell us what's up
  status  <- freshStatusPrint ## name "status"
  -- Create an entity that "debug" draws the physics system
  painter <- freshPhysicsDrawingEntity ## name "phys painter"
  -- Create all of our demo actors
  actors  <- makeDemoActors
  --options %= (S.insert SystemSkipPhysicsTick)

  -- Make a button to reset the demo
  bview   <- allocColorButtonView font "Reset" 16 buttonPainter (const $ return ())
  btn     <- freshButton 4 bview ## name "btn"
  recv (btnMailbox bview) $ \case
    -- When the button sends a 'clicked' message we reset the demo by destroying
    -- the actors, the button and then making them again
    ButtonStateClicked -> do
      mapM_ destroyEntity $ status:painter:btn:actors
      loop font
    st -> io $ print st

demo :: Font -> System ()
demo = loop
