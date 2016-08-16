{-# LANGUAGE FlexibleContexts #-}
module Odin.Scripts.Animation.Pic
  ( StreamOf
  , Animated
  , Animation
  , ColorAnimation
  , freshColorAnime
  ) where

import Gelatin.SDL2
import Control.Varying
import Control.Lens

import Odin.Core

type StreamOf a = Var Float a
type Animated a = SplineT Float a Identity
type Animation t s r v = Animated (PictureTransform, Picture t s r v ())
type ColorAnimation = Animation () (V2 Float) Float (V2 Float, V4 Float)

colorAnime :: Entity
        -> ColorAnimation ()
        -> System Script
colorAnime k a = do
  dt <- readTimeDeltaSeconds
  case runIdentity $ runSplineE a dt of
    (Left (t,pic), nxt) -> do setTfrmAndColorPic k t pic
                              nextScript $ colorAnime k nxt
    (Right (), _) -> endScript

freshColorAnime :: (Fresh s m
                   ,Scripts s m
                   ) => ColorAnimation () -> m Entity
freshColorAnime a = do
  k <- fresh
  let s = Script $ colorAnime k a
  (scripts.at k) %= Just . (maybe [s] (s:))
  return k
