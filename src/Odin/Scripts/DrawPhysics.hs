{-# LANGUAGE FlexibleContexts #-}
module Odin.Scripts.DrawPhysics where

import           Gelatin.SDL2
import qualified Data.IntMap as IM
import           Control.Monad (forM_)

import Odin.Common
import Odin.Component


--------------------------------------------------------------------------------
-- Converting a Scene into a Pic
--------------------------------------------------------------------------------
convexHullPic :: ConvexHull -> Pic
convexHullPic = f . map (fmap realToFrac) . canonicalizeConvexHull
  where f [] = return ()
        f (p:ps) = withStroke [StrokeWidth 2
                              ,StrokeFeather 0.5
                              ] $ do
                     lineStart (p, red) $ do
                       forM_ ps $ \pp -> lineTo (pp, red)
                       lineTo (p, red)
                     lineStart (0, red) $ lineTo (V2 (maxX ps) 0, white)
        maxX = maximum . map x
        x (V2 a _) = a

worldObjPos :: WorldObj -> V2 Float
worldObjPos = (realToFrac <$>) . canonicalizeVec . _physObjPos . _worldPhysObj

worldObjRot :: WorldObj -> Float
worldObjRot = realToFrac . _physObjRotPos . _worldPhysObj

worldObjTfrm :: WorldObj -> Transform
worldObjTfrm obj = Transform (worldObjPos obj) 1 (worldObjRot obj)

worldObjPicTfrm :: WorldObj -> PictureTransform
worldObjPicTfrm obj = PictureTransform (worldObjTfrm obj) 1 1

worldObjPic :: WorldObj -> Pic
worldObjPic obj = withTransform t $ convexHullPic $ _worldShape obj
  where t = worldObjPicTfrm obj

scenePic :: OdinScene -> Pic
scenePic = sequence_ . (worldObjPic <$>) . _worldObjs . _scWorld
--------------------------------------------------------------------------------
-- Scripts - Fresh'ing, Rendering, dealloc'ing, etc
--------------------------------------------------------------------------------
drawPhysics :: (Modifies OdinScene r
               ,ModifiesComponent RenderIO r
               ,ModifiesComponent DeallocIO r
               ,Reads Rez r
               ,DoesIO r
               ) => Entity -> Cache IO PictureTransform -> Eff r Script
drawPhysics k cache = do
  rez <- ask
  pic <- scenePic <$> getScene
  (r, newCache) <- io $ do
    (rnd, newCache) <- compilePictureRenderer rez cache pic
    -- Dealloc stale resources
    let stale = cache `IM.difference` newCache
    sequence_ $ fst <$> stale
    return (snd rnd, newCache)
  k `setRenderer` r
  k `setDealloc` sequence_ (fst <$> newCache)
  nextScript $ drawPhysics k newCache

freshPhysicsDrawingEntity :: (MakesEntities r
                             ,ModifiesComponent PictureTransform r
                             ,Modifies [Script] r
                             ) => Eff r Entity
freshPhysicsDrawingEntity = do
  k <- fresh
  k `setPicTransform` mempty
  addScript $ drawPhysics k mempty
  return k
