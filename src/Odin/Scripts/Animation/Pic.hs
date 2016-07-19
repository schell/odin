{-# LANGUAGE FlexibleContexts #-}
module Odin.Scripts.Animation.Pic (StreamOf, Animated, Animation, freshAnimation) where

import Gelatin.SDL2
import Control.Varying
import Data.Functor.Identity

import Odin.Common
import Odin.Component

type StreamOf a = Var Float a
type Animated a = SplineT Float a Identity
type Animation = Animated (PictureTransform,Pic)

animate :: (ModifiesComponent PictureTransform r
           ,ModifiesComponent RenderIO r
           ,ModifiesComponent DeallocIO r
           ,ModifiesComponent [Script] r
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
    (Right (), _) -> do k `addScripts` [f]
                        endScript

freshAnimation :: (MakesEntities r
                  ,ModifiesComponent [Script] r
                  ) => Animation () -> (Entity -> Script) -> Eff r Entity
freshAnimation a whenDone = do
  k <- fresh
  k `addScript` animate k a mempty (whenDone k)
  return k
