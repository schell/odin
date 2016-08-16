{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Odin.Core.Component where

import           Gelatin
import           Gelatin.SDL2 hiding (E)
import           SDL hiding (Event, get, time)
import qualified Data.IntMap.Strict as IM
import           Data.Monoid ((<>))
import           Control.Lens
import           Linear.Affine (Point(..))

import           Odin.Core.Common as OC
--------------------------------------------------------------------------------
-- Entities
--------------------------------------------------------------------------------
fresh :: Fresh s m => m Int
fresh = do
  k <- use nextK
  nextK += 1
  return k
--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------
readTimeDeltaSeconds :: (Time s m, Fractional f) => m f
readTimeDeltaSeconds = (/1000) . fromIntegral <$> use (time.timeDelta)

withTiming :: (Time s m, Fractional f) => m a -> m (f, a)
withTiming f = do
  t0 <- readTimeDeltaSeconds
  a  <- f
  t1 <- readTimeDeltaSeconds
  return (t1 - t0, a)

newTime :: DoesIO m => m SystemTime
newTime = do
  t <- io ticks
  let tt = SystemTime { _timeLast  = t
                      , _timeDelta = 0
                      , _timeLeft  = 0
                      }
  return tt
----------------------------------------------------------------------------------
-- Rendering Pictures
----------------------------------------------------------------------------------
allocColorPicRenderer :: (Reads Rez m, DoesIO m) => ColorPicture a -> m GLRenderer
allocColorPicRenderer pic = do
  rz <- ask
  io $ compileColorPicture rz pic

allocTexturePicRenderer ::(Reads Rez m, DoesIO m) => TexturePicture a -> m GLRenderer
allocTexturePicRenderer pic = do
  rz <- ask
  io $ compileTexturePicture rz pic
----------------------------------------------------------------------------------
---- Deallocating (Renderings, whatevs)
----------------------------------------------------------------------------------
dealloc :: (Deallocs s m, DoesIO m) => Entity -> m ()
dealloc k = use (deallocs.at k) >>= io . sequence_
--------------------------------------------------------------------------------
-- Destroying an entire entity and all its components
--------------------------------------------------------------------------------
destroyEntity :: Commands s m => Entity -> m ()
destroyEntity = (commands %=) . (:) . SystemDeleteEntity
--------------------------------------------------------------------------------
-- Common Helpers
--------------------------------------------------------------------------------
(.#) :: Monad m => Entity -> (m Entity -> m Entity) -> m Entity
k .# f = return k ## f

(##) :: m Entity -> (m Entity -> m Entity) -> m Entity
f ## g = g f

(#.) :: Monad m => m Entity -> (m Entity -> m Entity) -> m ()
f #. g = f ## g >> return ()

mkSetter :: Monad m => (b -> t -> m a) -> t -> m b -> m b
mkSetter w a f = do
  k <- f
  _ <- w k a
  return k

type SerialSetter m a = a -> m Entity -> m Entity

name :: Names s m => SerialSetter m Name
name = mkSetter setName
  where setName k n = names %= IM.insert k n

body :: Physics s m => SerialSetter m Body
body = mkSetter setBody
  where setBody k b = scene.scWorld.worldObjs %= IM.insert k (odinBodyToWorldObj b)

tfrm :: Tfrms s m => SerialSetter m PictureTransform
tfrm = mkSetter setTfrm
  where setTfrm k t = tfrms %= IM.insert k t

script :: Scripts s m => SerialSetter m [Script]
script = mkSetter setScripts
  where setScripts k s = scripts %= IM.insert k s

rndr :: Rndrs s m => SerialSetter m RenderIO
rndr = mkSetter setRenderer
  where setRenderer k r = rndrs %= IM.insert k r

dloc :: Deallocs s m => SerialSetter m DeallocIO
dloc = mkSetter setDealloc
  where setDealloc k d = deallocs %= IM.insert k d

colorPic :: (Rndrs s m, Deallocs s m, Reads Rez m, DoesIO m) => SerialSetter m (ColorPicture ())
colorPic = mkSetter setPic
  where setPic k p = do
          (c,r) <- allocColorPicRenderer p
          k .# rndr r
            ## dloc c

texPic :: (Rndrs s m, Deallocs s m, Reads Rez m, DoesIO m) => SerialSetter m (TexturePicture ())
texPic = mkSetter setPic
  where setPic k p = do
          (c,r) <- allocTexturePicRenderer p
          k .# rndr r
            ## dloc c

pos :: Tfrms s m => SerialSetter m (V2 Float)
pos = mkSetter setPos
  where setPos k v = do
          let t = PictureTransform (mat4Translate $ promoteV2 v) 1 1
          use (tfrms.at k) >>= \case
            Nothing -> tfrms.at k .= Just t
            Just t0 -> tfrms.at k .= (Just $ t <> t0)
--------------------------------------------------------------------------------
-- Less Common Helpers
--------------------------------------------------------------------------------
setTfrmAndColorPic :: (DoesIO m,Rndrs s m,Deallocs s m,Tfrms s m,Reads Rez m)
                   => Entity -> PictureTransform -> ColorPicture ()
                   -> m ()
setTfrmAndColorPic k t pic = do
  rz <- ask
  -- Compile the pic into a GLRenderer
  (c,r) <- do
    use (deallocs.at k) >>= \case
      Just c  -> io c
      Nothing -> return ()
    io $ compileColorPicture rz pic
  -- Update our components
  k .# tfrm t
    ## rndr r
    #. dloc c

setTfrmAndTexturePic :: (DoesIO m,Rndrs s m,Deallocs s m,Tfrms s m,Reads Rez m)
                   => Entity -> PictureTransform -> TexturePicture ()
                   -> m ()
setTfrmAndTexturePic k t pic = do
  rz <- ask
  -- Compile the pic into a GLRenderer
  (c,r) <- do
    use (deallocs.at k) >>= \case
      Just c  -> io c
      Nothing -> return ()
    io $ compileTexturePicture rz pic
  -- Update our components
  k .# tfrm t
    ## rndr r
    #. dloc c

getMouseIsOverEntityWithSize :: (DoesIO m
                                ,Tfrms s m
                                ) => Entity -> V2 Float -> m Bool
getMouseIsOverEntityWithSize k sz = do
  P vi  <- io getAbsoluteMouseLocation
  p <- use (tfrms.at k) >>= \case
    Nothing -> return mempty
    Just t  -> return t
  let vf = fromIntegral <$> vi
      bb = over both (transformV2' $ ptfrmMV p) (0,sz)
  return $ pointInBounds vf bb
