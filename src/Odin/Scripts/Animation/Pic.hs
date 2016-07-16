{-# LANGUAGE FlexibleContexts #-}
module Odin.Scripts.Animation.Pic (StreamOf, Animated, Animation, freshAnimation) where

import           Gelatin.SDL2
import           Control.Varying
import           Data.Functor.Identity
import qualified Data.IntMap as IM

import Odin.Common
import Odin.Component

type StreamOf a = Var Float a
type Animated a = SplineT Float a Identity
type Animation = Animated (PictureTransform,Pic)

setTfrmAndPic :: (DoesIO r
                 ,ModifiesComponent RenderIO r
                 ,ModifiesComponent DeallocIO r
                 ,ModifiesComponent PictureTransform r
                 ,Reads Rez r
                 ) => Entity -> PictureTransform -> Pic -> Cache IO PictureTransform -> Eff r (Cache IO PictureTransform)
setTfrmAndPic k tfrm pic cache = do
  rez <- ask
  -- Compile the pic into a RenderIO and a new resource cache
  (r, newCache) <- io $ do
    (rnd, newCache) <- compilePictureRenderer rez cache pic
    -- Dealloc the stale resources
    let stale = cache `IM.difference` newCache
    sequence_ $ fst <$> stale
    return (snd rnd, newCache)

  -- Update our components
  k `setPicTransform` tfrm
  k `setRenderer` r
  k `setDealloc` sequence_ (fst <$> newCache)
  return newCache

animate :: (ModifiesComponent PictureTransform r
           ,ModifiesComponent RenderIO r
           ,ModifiesComponent DeallocIO r
           ,Modifies [Script] r
           ,Modifies Time r
           ,Reads Rez r
           ,DoesIO r
           ) => Entity
             -> Animation ()
             -> Cache IO PictureTransform
             -> Script
             -> Eff r Script
animate k a cache f = do
  dt <- getTimeDelta
  case runIdentity $ runSplineE a dt of
    (Left (tfrm,pic), nxt) -> do newCache <- setTfrmAndPic k tfrm pic cache
                                 nextScript $ animate k nxt newCache f
    (Right (), _) -> do addScripts [f]
                        endScript

freshAnimation :: (MakesEntities r
                  ,Modifies [Script] r
                  ) => Animation () -> (Entity -> Script) -> Eff r Entity
freshAnimation a whenDone = do
  k <- fresh
  addScript $ animate k a mempty $ whenDone k
  return k
