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
import           SDL hiding (Event, get)
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import qualified Data.Set as S
import           Data.Word (Word32)
--import           Data.Set (Set)
import           Data.Monoid ((<>))

import           Odin.Core.Common
import           Odin.Core.Utils
--------------------------------------------------------------------------------
-- Entities
--------------------------------------------------------------------------------
fresh :: MakesEntities m => m Int
fresh = do
  k <- get
  put $ k+1
  return k
--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------
readTimeDelta :: Reads Time m => m Word32
readTimeDelta = timeDelta <$> ask

readTimeDeltaSeconds :: (Reads Time m, Fractional f) => m f
readTimeDeltaSeconds = ((/1000) . fromIntegral) <$> readTimeDelta

getTime :: Modifies Time m => m Time
getTime = get

putTime :: Modifies Time m => Time -> m ()
putTime = put
--------------------------------------------------------------------------------
-- PictureTransform
--------------------------------------------------------------------------------
getPicTransforms :: ModifiesComponent PictureTransform m
              => m (IntMap PictureTransform)
getPicTransforms = get

modifyPicTransforms :: ModifiesComponent PictureTransform m
                    => (IntMap PictureTransform -> IntMap PictureTransform)
                    -> m ()
modifyPicTransforms = modify

setPicTransform :: ModifiesComponent PictureTransform m
             => Entity -> PictureTransform -> m ()
setPicTransform k p = modify (IM.insert k p)

getPicTransform :: ModifiesComponent PictureTransform m
             => Entity -> m (Maybe PictureTransform)
getPicTransform k = IM.lookup k <$> get

modifyPicTransform :: ModifiesComponent PictureTransform m
                => Entity -> (PictureTransform -> PictureTransform) -> m ()
modifyPicTransform k f =
  getPicTransform k >>= \case
    Nothing -> setPicTransform k $ f mempty
    Just t  -> setPicTransform k $ f t

deletePicTransform :: ModifiesComponent PictureTransform m => Entity -> m ()
deletePicTransform k = modifyPicTransforms $ IM.delete k

movePicTransform :: ModifiesComponent PictureTransform m
                 => Entity -> V2 Float -> m ()
movePicTransform k v = modifyPicTransform k $ \(PictureTransform t a m) ->
  PictureTransform (t<>Transform v 1 0) a m
--------------------------------------------------------------------------------
-- Scripts
--------------------------------------------------------------------------------
getScripts :: ModifiesComponent [Script] m => Entity -> m (Maybe [Script])
getScripts k = IM.lookup k <$> get

addScript :: ModifiesComponent [Script] m => Entity -> ScriptStep -> m ()
addScript k f = getScripts k >>= modify . \case
  Nothing -> IM.insert k [Script f]
  Just ss -> IM.insert k (Script f:ss)

addScripts :: ModifiesComponent [Script] m => Entity -> [Script] -> m ()
addScripts k xs = getScripts k >>= modify . \case
  Nothing -> IM.insert k xs
  Just ys -> IM.insert k (xs ++ ys)

setScripts :: ModifiesComponent [Script] m => Entity -> [Script] -> m ()
setScripts k xs = modify $ IM.insert k xs

deleteScripts :: ModifiesComponent [Script] m => Entity -> m ()
deleteScripts = modify . del
  where del :: Entity -> IntMap [Script] -> IntMap [Script]
        del = IM.delete
--------------------------------------------------------------------------------
-- Physics
--------------------------------------------------------------------------------
getScene :: Modifies OdinScene m => m OdinScene
getScene = get

modifyScene :: Modifies OdinScene m => (OdinScene -> OdinScene) -> m ()
modifyScene = modify

getWorldObjects :: Modifies OdinScene m => m (IntMap WorldObj)
getWorldObjects = (_worldObjs . _scWorld) <$> getScene

setWorldObjects :: Modifies OdinScene m => IntMap WorldObj -> m ()
setWorldObjects objs = modifyScene $ \s ->
  let world = (_scWorld s){ _worldObjs = objs }
  in s{ _scWorld = world }

modifyWorldObjects :: Modifies OdinScene m
                   => (IntMap WorldObj -> IntMap WorldObj) -> m ()
modifyWorldObjects f = getWorldObjects >>= setWorldObjects . f

addWorldObject :: Modifies OdinScene m => Entity -> WorldObj -> m ()
addWorldObject k o = modifyWorldObjects $ IM.insert k o

setBody :: Modifies OdinScene m => Entity -> Body -> m ()
setBody k = addWorldObject k . odinBodyToWorldObj

deleteWorldObject :: Modifies OdinScene m => Entity -> m ()
deleteWorldObject k = modifyWorldObjects $ IM.delete k

deleteBody :: Modifies OdinScene m => Entity -> m ()
deleteBody = deleteWorldObject
--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------
setName :: ModifiesComponent Name m => Entity -> String -> m ()
setName k n = modify $ IM.insert k n

deleteName :: ModifiesComponent Name m => Entity -> m ()
deleteName k = modify (IM.delete k :: IntMap String -> IntMap String)
--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------
getEvents :: Modifies [EventPayload] m => m [EventPayload]
getEvents = get
--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------
getRenderers :: ModifiesComponent RenderIO m => m (IntMap RenderIO)
getRenderers = get

modifyRenderers :: ModifiesComponent RenderIO m
                => (IntMap RenderIO -> IntMap RenderIO) -> m ()
modifyRenderers = modify

setRenderer :: ModifiesComponent RenderIO m
            => Entity -> RenderIO -> m ()
setRenderer k r = modify (IM.insert k r)

getRenderer :: ModifiesComponent RenderIO m
            => Entity -> m (Maybe RenderIO)
getRenderer k = IM.lookup k <$> get

deleteRenderer :: ModifiesComponent RenderIO m
               => Entity -> m ()
deleteRenderer k = modifyRenderers (IM.delete k)

allocPicRenderer ::(Reads Rez m
                   ,DoesIO m
                   ) => Pic -> m GLRenderer
allocPicRenderer pic = do
  rez <- ask
  io $ compilePic rez pic

setPicRenderer :: (ModifiesComponent RenderIO m
                  ,Reads Rez m
                  ,DoesIO m
                  ) => Entity -> Pic -> m DeallocIO
setPicRenderer k pic = do
  r <- allocPicRenderer pic
  k `setRenderer` snd r
  return $ fst r
--------------------------------------------------------------------------------
-- Deallocating (Renderings, whatevs)
--------------------------------------------------------------------------------
getDealloc :: ModifiesComponent DeallocIO m
           => Entity -> m (Maybe DeallocIO)
getDealloc k = IM.lookup k <$> get

setDealloc :: ModifiesComponent DeallocIO m
           => Entity -> DeallocIO -> m ()
setDealloc k d = modify $ IM.insert k d

dealloc :: (ModifiesComponent DeallocIO m
           ,DoesIO m
           ) => Entity -> m ()
dealloc k = getDealloc k >>= io . sequence_

deleteDealloc :: ModifiesComponent DeallocIO m => Entity -> m ()
deleteDealloc = modify . del
  where del = IM.delete :: Entity -> IntMap DeallocIO -> IntMap DeallocIO
--------------------------------------------------------------------------------
-- System Options
--------------------------------------------------------------------------------
getOptions :: Modifies SystemOptions m => m SystemOptions
getOptions = get

setOption :: Modifies SystemOptions m => SystemOption -> m ()
setOption = modify . S.insert

clearOption :: Modifies SystemOptions m => SystemOption -> m ()
clearOption = modify . S.delete

optionIsSet :: Modifies SystemOptions m => SystemOption -> m Bool
optionIsSet o = S.member o <$> getOptions
--------------------------------------------------------------------------------
-- Destroying an entire entity and all its components
--------------------------------------------------------------------------------
destroyEntity :: Modifies SystemCommands m => Entity -> m ()
destroyEntity = modify . (:) . SystemDeleteEntity
--------------------------------------------------------------------------------
-- Common Helpers
--------------------------------------------------------------------------------
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

name :: ModifiesComponent Name m => SerialSetter m Name
name = mkSetter setName

body :: Modifies OdinScene m => SerialSetter m Body
body = mkSetter setBody

tfrm :: ModifiesComponent PictureTransform m => SerialSetter m PictureTransform
tfrm = mkSetter setPicTransform

scripts :: ModifiesComponent [Script] m => SerialSetter m [Script]
scripts = mkSetter setScripts

rndr :: ModifiesComponent RenderIO m => SerialSetter m RenderIO
rndr = mkSetter setRenderer

dloc :: ModifiesComponent DeallocIO m => SerialSetter m DeallocIO
dloc = mkSetter setDealloc
--------------------------------------------------------------------------------
-- Less Common Helpers
--------------------------------------------------------------------------------
setTfrmAndPic :: (DoesIO m
                 ,ModifiesComponent RenderIO m
                 ,ModifiesComponent DeallocIO m
                 ,ModifiesComponent PictureTransform m
                 ,Reads Rez m
                 ) => Entity -> PictureTransform -> Pic -> Cache IO PictureTransform -> m (Cache IO PictureTransform)
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
