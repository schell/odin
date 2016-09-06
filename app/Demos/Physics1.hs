{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Demos.Physics1 ( demo ) where

import Control.Lens hiding (to)
import Linear hiding (rotate)

import Gelatin hiding (move,rotate)
import Odin.Core
import Odin.GUI

data Demo m = Demo { demoBiggy         :: Int
                   , demoRightWall     :: Int
                   , demoBottomWall    :: Int
                   , demoSquares       :: [Int]
                   , demoRndSquare     :: RenderGUI (UpdateT m) ()
                   , demoRndBiggy      :: RenderGUI (UpdateT m) ()
                   , demoRndRightWall  :: RenderGUI (UpdateT m) ()
                   , demoRndBottomWall :: RenderGUI (UpdateT m) ()
                   }

withDemo :: MonadIO m => (Demo m -> UpdateT m ()) -> UpdateT m ()
withDemo f = do
  blankScene <- use scene
  -- Create our physics bodies
  biggy <- fresh ## body emptyBody{ bRotVel = 1
                                  , bPos    = (180,180)
                                  , bHull   = rectangleHull 10 30
                                  }
  rwall <- fresh ## body emptyBody{ bPos    = (300, 150)
                                  , bHull   = rectangleHull 10 289
                                  }
  bwall <- fresh ## body emptyBody{ bPos    = (150, 300)
                                  , bHull   = rectangleHull 289 10
                                  }
  squares <- forM [(x,y) | y <- [0..5], x <- [0..5]] $ \(x,y) -> do
    let tx = 100 + x*11
        ty = 100 + y*11
    fresh ## body emptyBody{ bVel    = (5,5)
                           , bPos    = (tx, ty)
                           , bMass   = (1,1)
                           , bHull   = rectangleHull 10 10
                           }
  -- A function to draw our physics actors
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
            to (V2 (w/2)       0, r5)
            to (V2 (w/2)   (h/2), r5)
            to (V2 (-w/2)  (h/2), r5)
            to (V2 (-w/2) (-h/2), r5)
            to (V2 (w/2)  (-h/2), r5)
            to (V2 (w/2)       0, r5)
  -- Alloc our renderers
  withColorPicture (actorPic 10 30) $ \(_,biggyr) ->
    withColorPicture (actorPic 10 289) $ \(_,rwallr) ->
      withColorPicture (actorPic 289 10) $ \(_,bwallr) ->
        withColorPicture (actorPic 10 10) $ \(_,squarer) ->
          f Demo{ demoBiggy         = biggy
                , demoRightWall     = rwall
                , demoBottomWall    = bwall
                , demoSquares       = squares
                , demoRndSquare     = squarer
                , demoRndBiggy      = biggyr
                , demoRndRightWall  = rwallr
                , demoRndBottomWall = bwallr
                }
  -- Replace the scene
  scene .= blankScene

renderMObj :: Monad m => RenderGUI (UpdateT m) () -> Maybe WorldObj -> UpdateT m ()
renderMObj _ Nothing = return ()
renderMObj r (Just obj) = r [moveV2 $ worldObjPos obj, rotate $ worldObjRot obj]

renderDemo :: MonadIO m => Demo m -> UpdateT m ()
renderDemo Demo{..} = do
  -- Get the position and rotation of our bodies
  mbiggyObj <- use (scene.scWorld.worldObjs.at demoBiggy)
  mrwallObj <- use (scene.scWorld.worldObjs.at demoRightWall)
  mbwallObj <- use (scene.scWorld.worldObjs.at demoBottomWall)
  msObjs    <- forM demoSquares $ \k -> use (scene.scWorld.worldObjs.at k)
  -- Render them
  renderMObj demoRndBiggy      mbiggyObj
  renderMObj demoRndRightWall  mrwallObj
  renderMObj demoRndBottomWall mbwallObj
  mapM_ (renderMObj demoRndSquare) msObjs

demo :: MonadIO m => UpdateT m ()
demo = do
  withDefaultStatusBar white $ \status -> do
    withDefaultButton "Reset" $ \reset -> do
      V2 btnw _ <- sizeOfButton reset
      withDefaultText white "Physics1 demo" $ \title -> fix $ \resetTask -> do
        withDemo $ \d -> fix $ \frame -> do
          btnState <- renderButton reset []
          renderText title [move btnw 16]
          renderStatusBar status [move 0 320]
          if btnState == ButtonStateClicked
            then return ()
            else do renderDemo d
                    next frame
        next resetTask
