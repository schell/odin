{-# LANGUAGE FlexibleContexts #-}
module Odin.Scripts.DrawPhysics where

import           Gelatin.SDL2
import qualified Data.IntMap as IM
import           Control.Lens hiding (to)

import Odin.Core

--------------------------------------------------------------------------------
-- Converting a Scene into a Pic
--------------------------------------------------------------------------------
convexHullPic :: ConvexHull -> ColorPicture ()
convexHullPic hull = do
  case map (fmap realToFrac) $ canonicalizeConvexHull hull of
    []     -> return ()
    (p:ps) -> do setStroke [StrokeWidth 2, StrokeFeather 0.5]
                 setGeometry $ geometry $ do
                   add $ line $ vertices $ do
                     to (p, red)
                     forM_ ps $ \pp -> to (pp, red)
                     to (p, red)
                   add $ line $ vertices $ do
                     let maxX = maximum . map x
                         x (V2 a _) = a
                     to (0, red)
                     to (V2 (maxX ps) 0, white)

worldObjPos :: WorldObj -> V2 Float
worldObjPos = (realToFrac <$>) . canonicalizeVec . _physObjPos . _worldPhysObj

worldObjRot :: WorldObj -> Float
worldObjRot = realToFrac . _physObjRotPos . _worldPhysObj

worldObjPic :: WorldObj -> ColorPicture ()
worldObjPic obj = do
  move v
  rotate r
  convexHullPic $ _worldShape obj
  where v = worldObjPos obj
        r = worldObjRot obj

scenePic :: OdinScene -> ColorPicture ()
scenePic = mapM_ (embed . worldObjPic) . IM.elems . (^.scWorld.worldObjs)
--------------------------------------------------------------------------------
-- Scripts - Fresh'ing, Rendering, dealloc'ing, etc
--------------------------------------------------------------------------------
drawPhysics :: (Physics s m, Rndrs s m, Deallocs s m, Reads Rez m, DoesIO m)
            => Entity -> m Script
drawPhysics k = do
  -- Dealloc the last frame
  use (deallocs.at k) >>= (maybe (return ()) io)
  -- Alloc the new frame
  rz <- ask
  pic <- scenePic <$> use scene
  (c,r) <- io $ compileColorPicture rz pic
  -- Update entity values
  k .# rndr r
    #. dloc c
  nextScript $ drawPhysics k

freshPhysicsDrawingEntity :: (Fresh s m, Tfrms s m, Scripts s m) => m Entity
freshPhysicsDrawingEntity = do
  k <- fresh
  k .# tfrm mempty
    ## script [Script $ drawPhysics k]
