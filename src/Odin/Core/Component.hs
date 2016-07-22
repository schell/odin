{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Odin.Core.Component where

import           Gelatin.Picture
import           Gelatin.SDL2 hiding (E)
import           SDL hiding (Event, get, time)
import qualified Data.IntMap.Strict as IM
import           Data.Monoid ((<>))
import           Control.Lens
import           Linear.Affine (Point(..))

import           Odin.Core.Common as OC
import           Odin.Core.Utils
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

newTime :: DoesIO m => m SystemTime
newTime = do
  t <- io ticks
  let tt = SystemTime { _timeLast  = t
                      , _timeDelta = 0
                      , _timeLeft  = 0
                      }
  return tt
--------------------------------------------------------------------------------
-- PictureTransform
--------------------------------------------------------------------------------
--modifyPicTransforms :: Tfrms s m
--                    => (IntMap PictureTransform -> IntMap PictureTransform)
--                    -> m ()
--modifyPicTransforms = modify
--
--setPicTransform :: Tfrms s m
--             => Entity -> PictureTransform -> m ()
--setPicTransform k p = modify (IM.insert k p)
--
--readPicTransform :: Reads (IntMap PictureTransform) m
--             => Entity -> m (Maybe PictureTransform)
--readPicTransform k = IM.lookup k <$> ask
--
--modifyPicTransform :: Tfrms s m
--                => Entity -> (PictureTransform -> PictureTransform) -> m ()
--modifyPicTransform k f =
--  readPicTransform k >>= \case
--    Nothing -> setPicTransform k $ f mempty
--    Just t  -> setPicTransform k $ f t
--
--deletePicTransform :: Tfrms s m => Entity -> m ()
--deletePicTransform k = modifyPicTransforms $ IM.delete k
--
--movePicTransform :: Tfrms s m
--                 => Entity -> V2 Float -> m ()
--movePicTransform k v = modifyPicTransform k $ \(PictureTransform t a m) ->
--  PictureTransform (t<>Transform v 1 0) a m
----------------------------------------------------------------------------------
---- Scripts
----------------------------------------------------------------------------------
--getScripts :: [Script] m => Entity -> s m (Maybe [Script])
--getScripts k = IM.lookup k <$> get
--
--addScript :: [Script] m => Entity -> ScriptStep -> s m ()
--addScript k f = getScripts k >>= modify . \case
--  Nothing -> IM.insert k [Script f]
--  Just ss -> IM.insert k (Script f:ss)
--
--addScripts :: [Script] m => Entity -> [Script] -> s m ()
--addScripts k xs = getScripts k >>= modify . \case
--  Nothing -> IM.insert k xs
--  Just ys -> IM.insert k (xs ++ ys)
--
--setScripts :: [Script] m => Entity -> [Script] -> s m ()
--setScripts k xs = modify $ IM.insert k xs
--
--deleteScripts :: [Script] m => Entity -> s m ()
--deleteScripts = modify . del
--  where del :: Entity -> IntMap [Script] -> IntMap [Script]
--        del = IM.delete
----------------------------------------------------------------------------------
---- Physics
----------------------------------------------------------------------------------
--getWorldObjects :: Physics s m => m (IntMap WorldObj)
--getWorldObjects = do
--  s <- use scene
--  return $ s^.scWorld.worldObjs
--
--setWorldObjects :: OdinScene m => IntMap WorldObj -> s m ()
--setWorldObjects objs = modifyScene $ \s ->
--  let world = (_scWorld s){ _worldObjs = objs }
--  in s{ _scWorld = world }
--
--modifyWorldObjects :: OdinScene s m
--                   => (IntMap WorldObj -> IntMap WorldObj) -> m ()
--modifyWorldObjects f = getWorldObjects >>= setWorldObjects . f
--
--addWorldObject :: OdinScene m => Entity -> WorldObj -> s m ()
--addWorldObject k o = modifyWorldObjects $ IM.insert k o
--
--setBody :: OdinScene m => Entity -> Body -> s m ()
--setBody k b = scene.scWorld.worldObjs %= IM.insert k (odinBodyToWorldObj b)
--
--deleteWorldObject :: OdinScene m => Entity -> s m ()
--deleteWorldObject k = modifyWorldObjects $ IM.delete k
--
--deleteBody :: OdinScene m => Entity -> s m ()
--deleteBody = deleteWorldObject
----------------------------------------------------------------------------------
---- Names
----------------------------------------------------------------------------------
--setName :: Name m => Entity -> String -> s m ()
--setName k n = modify $ IM.insert k n
--
--deleteName :: Name m => Entity -> s m ()
--deleteName k = modify (IM.delete k :: IntMap String -> IntMap String)
----------------------------------------------------------------------------------
---- Events
----------------------------------------------------------------------------------
--getEvents :: [EventPayload] m => s m [EventPayload]
--getEvents = get
----------------------------------------------------------------------------------
---- Rendering
----------------------------------------------------------------------------------
allocPicRenderer ::(Reads Rez m, DoesIO m) => Pic -> m GLRenderer
allocPicRenderer pic = do
  rz <- ask
  io $ compilePic rz pic
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

pictr :: (Rndrs s m, Deallocs s m, Reads Rez m, DoesIO m) => SerialSetter m Pic
pictr = mkSetter setPic
  where setPic k p = do
          (c,r) <- allocPicRenderer p
          k .# rndr r
            ## dloc c

pos :: Tfrms s m => SerialSetter m (V2 Float)
pos = mkSetter setPos
  where setPos k v = do
          let t = PictureTransform (Transform v 1 0) 1 1
          use (tfrms.at k) >>= \case
            Nothing -> tfrms.at k .= Just t
            Just t0 -> tfrms.at k .= (Just $ t <> t0)
--------------------------------------------------------------------------------
-- Less Common Helpers
--------------------------------------------------------------------------------
setTfrmAndPic :: (DoesIO m,Rndrs s m,Deallocs s m,Tfrms s m,Reads Rez m)
              => Entity -> PictureTransform -> Pic -> Cache IO PictureTransform
              -> m (Cache IO PictureTransform)
setTfrmAndPic k t pic cache = do
  rz <- ask
  -- Compile the pic into a RenderIO and a new resource cache
  (r, newCache) <- io $ do
    (rnd, newCache) <- compilePictureRenderer rz cache pic
    -- Dealloc the stale resources
    let stale = cache `IM.difference` newCache
    sequence_ $ fst <$> stale
    return (snd rnd, newCache)

  -- Update our components
  k .# tfrm t
    ## rndr r
    #. dloc (sequence_ (fst <$> newCache))
  return newCache

getMouseIsOverEntityWithSize :: (DoesIO m
                                ,Tfrms s m
                                ) => Entity -> V2 Float -> m Bool
getMouseIsOverEntityWithSize k sz = do
  P vi  <- io getAbsoluteMouseLocation
  ptfrm <- use (tfrms.at k) >>= \case
    Nothing -> return mempty
    Just t  -> return t
  let vf = fromIntegral <$> vi
      bb = applyPicTfrmToBounds ptfrm (0,sz)
  return $ pointInBounds vf bb
