{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Demos.Physics
  ( PhysicsDemo(..)
  , allocDemo
  , renderDemo
  , freeDemo
  ) where

import Control.Lens hiding (to)
import Control.Monad.State.Strict
import Linear hiding (rotate)

import Gelatin hiding (move,rotate)
import Gelatin.GL hiding (move,rotate)
import Odin.Core
import Odin.GUI
import Odin.GUI.Text.Internal
import Odin.GUI.Button.Internal

data DemoData = DemoData { _demoBiggy         :: Int
                         , _demoRightWall     :: Int
                         , _demoBottomWall    :: Int
                         , _demoSquares       :: [Int]
                         , _demoScene         :: OdinScene
                         , _demoPaused        :: Bool
                         , _demoTime          :: SystemTime
                         , _demoNextK         :: Int
                         , _demoTitle         :: Slot Text
                         , _demoResetBtn      :: Slot Button
                         , _demoRndSquare     :: Slot GUIRenderer
                         , _demoRndBiggy      :: Slot GUIRenderer
                         , _demoRndRightWall  :: Slot GUIRenderer
                         , _demoRndBottomWall :: Slot GUIRenderer
                         }
makeLenses ''DemoData

type DemoT m = StateT DemoData m

instance HasNextK DemoData Int where
  nextK = demoNextK
instance HasScene DemoData OdinScene where
  scene = demoScene
instance HasTime DemoData SystemTime where
  time = demoTime

emptyDemoData :: Slot Text -> Slot Button
              -> Slot GUIRenderer
              -> Slot GUIRenderer
              -> Slot GUIRenderer
              -> Slot GUIRenderer
              -> DemoData
emptyDemoData txt btn sq big rw bw =
  DemoData { _demoBiggy         = 0
           , _demoRightWall     = 0
           , _demoBottomWall    = 0
           , _demoSquares       = []
           , _demoScene         = emptyScene
           , _demoPaused        = False
           , _demoTime          = SystemTime 0 0 0
           , _demoNextK         = 0
           , _demoTitle         = txt
           , _demoResetBtn      = btn
           , _demoRndSquare     = sq
           , _demoRndBiggy      = big
           , _demoRndRightWall  = rw
           , _demoRndBottomWall = bw
           }

setActors :: MonadIO m => DemoT m ()
setActors = do
  biggy <- fresh ## body emptyBody{ bRotVel = 1
                                  , bPos    = (180,180)
                                  , bHull   = rectangleHull 10 30
                                  }
  demoBiggy .= biggy

  rwall <- fresh ## body emptyBody{ bPos    = (300, 150)
                                  , bHull   = rectangleHull 10 289
                                  }
  demoRightWall .= rwall

  bwall <- fresh ## body emptyBody{ bPos    = (150, 300)
                                  , bHull   = rectangleHull 289 10
                                  }
  demoBottomWall .= bwall

  squares <- forM [(x,y) | y <- [0..5], x <- [0..5]] $ \(x,y) -> do
    let tx = 100 + x*11
        ty = 100 + y*11
    fresh ## body emptyBody{ bVel    = (5,5)
                           , bPos    = (tx, ty)
                           , bMass   = (1,1)
                           , bHull   = rectangleHull 10 10
                           }
  demoSquares .= squares

resetPhysics :: MonadIO m => DemoT m ()
resetPhysics = do
  demoScene .= emptyScene
  setActors

-- A function to draw each actor. Used to alloc all our renderers.
actorPic :: Monad m => Float -> Float -> ColorPictureT m ()
actorPic w h = do
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

renderMObj :: MonadIO m
           => Slot GUIRenderer -> [RenderTransform] -> Maybe WorldObj -> m ()
renderMObj _ _ Nothing = return ()
renderMObj s rs (Just obj) = do
  -- Transform the renderings by the bodys' position and rotation
  (_,r) <- unslot s
  io $ r $ rs ++ [moveV2 $ worldObjPos obj, rotate $ worldObjRot obj]

renderDemoData :: MonadIO m => [RenderTransform] -> DemoT m ()
renderDemoData rs = do
  reset     <- use demoResetBtn
  title     <- use demoTitle
  V2 btnw _ <- sizeOfButton reset
  btnState <- renderButton reset rs
  renderText title $ [move btnw 16] ++ rs

  when (btnState == ButtonStateClicked) resetPhysics

  biggy   <- use demoBiggy
  rwall   <- use demoRightWall
  bwall   <- use demoBottomWall
  squares <- use demoSquares

  -- Get the bodies out of our scene
  mbiggyObj <- use (scene.scWorld.worldObjs.at biggy)
  mrwallObj <- use (scene.scWorld.worldObjs.at rwall)
  mbwallObj <- use (scene.scWorld.worldObjs.at bwall)
  msObjs    <- mapM (\k -> use (scene.scWorld.worldObjs.at k)) squares
  -- Render them
  rbiggy  <- use demoRndBiggy
  rrwall  <- use demoRndRightWall
  rbwall  <- use demoRndBottomWall
  rsquare <- use demoRndSquare
  lift $ do renderMObj rbiggy rs mbiggyObj
            renderMObj rrwall rs mrwallObj
            renderMObj rbwall rs mbwallObj
            mapM_ (renderMObj rsquare rs) msObjs

renderPhysDemo :: MonadIO m => Slot DemoData -> [RenderTransform] -> m ()
renderPhysDemo s rs = modifySlotM s $ execStateT $ do
  tickTime
  paused <- use demoPaused
  unless paused tickPhysics
  renderDemoData rs

freeDemoData :: MonadIO m => Slot DemoData -> m ()
freeDemoData s = do
  DemoData{..} <- unslot s
  freeText    _demoTitle
  freeButton  _demoResetBtn
  fromSlotM _demoRndSquare     (io . fst)
  fromSlotM _demoRndBiggy      (io . fst)
  fromSlotM _demoRndRightWall  (io . fst)
  fromSlotM _demoRndBottomWall (io . fst)

addRenderers :: (MonadIO m, Rezed s m, Resources s m) => DemoT m ()
addRenderers = do
  biggy  <- lift (snd <$> allocColorPicture (actorPic 10 30))
  rwall  <- lift (snd <$> allocColorPicture (actorPic 10 289))
  bwall  <- lift (snd <$> allocColorPicture (actorPic 289 10))
  square <- lift (snd <$> allocColorPicture (actorPic 10 10))
  demoRndSquare     .= square
  demoRndBiggy      .= biggy
  demoRndRightWall  .= rwall
  demoRndBottomWall .= bwall

pauseDemoData :: MonadIO m => Slot DemoData -> m ()
pauseDemoData s = modifySlotM s $ execStateT (demoPaused .= True)

resumeDemoData :: MonadIO m => Slot DemoData -> m ()
resumeDemoData s = modifySlotM s $ execStateT $ do
  tickTime
  demoPaused .= False

data PhysicsDemo m = PhysicsDemo { demoRenderer :: GUIRenderer
                                 , pauseDemo    :: m ()
                                 , resumeDemo   :: m ()
                                 }

allocDemo :: (MonadIO m, Resources s m, Rezed s m, Fonts s m)
          => m (PhysicsDemo m)
allocDemo = do
  comicFont <- getFontPath "KMKDSP__.ttf"
  let desc = fontDescriptor comicFont 16
  title <- allocText desc white "Physics Demo"
  reset <- allocButton buttonPainter "Reset"
  let emptyGUI = (return (), const $ return ())
  sq    <- slot emptyGUI
  big   <- slot emptyGUI
  rw    <- slot emptyGUI
  bw    <- slot emptyGUI
  flip evalStateT (emptyDemoData title reset sq big rw bw) $ do
    t <- io newTime
    demoTime .= t
    addRenderers
    resetPhysics
    s <- slot =<< get
    return
      PhysicsDemo { demoRenderer = (freeDemoData s, renderPhysDemo s)
                  , pauseDemo = pauseDemoData s
                  , resumeDemo = resumeDemoData s
                  }

renderDemo :: MonadIO m => PhysicsDemo m -> [RenderTransform] -> m ()
renderDemo d rs = io $ (snd $ demoRenderer d) rs

freeDemo :: MonadIO m => PhysicsDemo m -> m ()
freeDemo = io . fst . demoRenderer
