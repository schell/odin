{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Odin.Component where

import           Gelatin.Picture
import           Gelatin.SDL2 hiding (E)
import           SDL hiding (Event, get)
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import qualified Data.Set as S
--import           Data.Set (Set)
import           Data.Monoid ((<>))

import           Odin.Common
import           Odin.Utils
--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------
getTimeDelta :: Modifies Time r => Eff r Float
getTimeDelta = timeDelta <$> get
--------------------------------------------------------------------------------
-- PictureTransform
--------------------------------------------------------------------------------
getPicTransforms :: Member (State (IntMap PictureTransform)) r
              => Eff r (IntMap PictureTransform)
getPicTransforms = get

modifyPicTransforms :: ModifiesComponent PictureTransform r
                    => (IntMap PictureTransform -> IntMap PictureTransform)
                    -> Eff r ()
modifyPicTransforms = modify

setPicTransform :: ModifiesComponent PictureTransform r
             => Entity -> PictureTransform -> Eff r ()
setPicTransform k p = modify (IM.insert k p)

getPicTransform :: ModifiesComponent PictureTransform r
             => Entity -> Eff r (Maybe PictureTransform)
getPicTransform k = IM.lookup k <$> get

modifyPicTransform :: ModifiesComponent PictureTransform r
                => Entity -> (PictureTransform -> PictureTransform) -> Eff r ()
modifyPicTransform k f =
  getPicTransform k >>= \case
    Nothing -> setPicTransform k $ f mempty
    Just t  -> setPicTransform k $ f t

deletePicTransform :: ModifiesComponent PictureTransform r => Entity -> Eff r ()
deletePicTransform k = modifyPicTransforms $ IM.delete k

movePicTransform :: ModifiesComponent PictureTransform r
                 => Entity -> V2 Float -> Eff r ()
movePicTransform k v = modifyPicTransform k $ \(PictureTransform t a m) ->
  PictureTransform (t<>Transform v 1 0) a m
--------------------------------------------------------------------------------
-- Scripts
--------------------------------------------------------------------------------
getScripts :: ModifiesComponent [Script] r => Entity -> Eff r (Maybe [Script])
getScripts k = IM.lookup k <$> get

addScript :: ModifiesComponent [Script] r => Entity -> ScriptStep -> Eff r ()
addScript k f = getScripts k >>= modify . \case
  Nothing -> IM.insert k [Script f]
  Just ss -> IM.insert k (Script f:ss)

addScripts :: ModifiesComponent [Script] r => Entity -> [Script] -> Eff r ()
addScripts k xs = getScripts k >>= modify . \case
  Nothing -> IM.insert k xs
  Just ys -> IM.insert k (xs ++ ys)

deleteScripts :: ModifiesComponent [Script] r => Entity -> Eff r ()
deleteScripts = modify . del
  where del :: Entity -> IntMap [Script] -> IntMap [Script]
        del = IM.delete
--------------------------------------------------------------------------------
-- Physics
--------------------------------------------------------------------------------
getScene :: Modifies OdinScene r => Eff r OdinScene
getScene = get

modifyScene :: Modifies OdinScene r => (OdinScene -> OdinScene) -> Eff r ()
modifyScene = modify

getWorldObjects :: Modifies OdinScene r => Eff r (IntMap WorldObj)
getWorldObjects = (_worldObjs . _scWorld) <$> getScene

setWorldObjects :: Modifies OdinScene r => IntMap WorldObj -> Eff r ()
setWorldObjects objs = modifyScene $ \s ->
  let world = (_scWorld s){ _worldObjs = objs }
  in s{ _scWorld = world }

modifyWorldObjects :: Modifies OdinScene r
                   => (IntMap WorldObj -> IntMap WorldObj) -> Eff r ()
modifyWorldObjects f = getWorldObjects >>= setWorldObjects . f

addWorldObject :: Modifies OdinScene r => Entity -> WorldObj -> Eff r ()
addWorldObject k o = modifyWorldObjects $ IM.insert k o

addOdinObject :: Modifies OdinScene r => Entity -> OdinObject -> Eff r ()
addOdinObject k = addWorldObject k . odinObjectToWorldObj

deleteWorldObject :: Modifies OdinScene r => Entity -> Eff r ()
deleteWorldObject k = modifyWorldObjects $ IM.delete k

deleteOdinObject :: Modifies OdinScene r => Entity -> Eff r ()
deleteOdinObject = deleteWorldObject
--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------
setName :: ModifiesComponent Name r => Entity -> String -> Eff r ()
setName k n = modify $ IM.insert k n

deleteName :: ModifiesComponent Name r => Entity -> Eff r ()
deleteName k = modify (IM.delete k :: IntMap String -> IntMap String)
--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------
getEvents :: Modifies [EventPayload] r => Eff r [EventPayload]
getEvents = get
--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------
getRenderers :: ModifiesComponent RenderIO r => Eff r (IntMap RenderIO)
getRenderers = get

modifyRenderers :: ModifiesComponent RenderIO r
                => (IntMap RenderIO -> IntMap RenderIO) -> Eff r ()
modifyRenderers = modify

setRenderer :: ModifiesComponent RenderIO r
            => Entity -> RenderIO -> Eff r ()
setRenderer k r = modify (IM.insert k r)

getRenderer :: ModifiesComponent RenderIO r
            => Entity -> Eff r (Maybe RenderIO)
getRenderer k = IM.lookup k <$> get

deleteRenderer :: ModifiesComponent RenderIO r
               => Entity -> Eff r ()
deleteRenderer k = modifyRenderers (IM.delete k)

allocPicRenderer ::(Reads Rez r
                   ,DoesIO r
                   ) => Pic -> Eff r GLRenderer
allocPicRenderer pic = do
  rez <- ask
  io $ compilePic rez pic

setPicRenderer :: (ModifiesComponent RenderIO r
                  ,Reads Rez r
                  ,DoesIO r
                  ) => Entity -> Pic -> Eff r DeallocIO
setPicRenderer k pic = do
  r <- allocPicRenderer pic
  k `setRenderer` snd r
  return $ fst r
--------------------------------------------------------------------------------
-- Deallocating (Renderings, whatevs)
--------------------------------------------------------------------------------
getDealloc :: ModifiesComponent DeallocIO r
           => Entity -> Eff r (Maybe DeallocIO)
getDealloc k = IM.lookup k <$> get

setDealloc :: ModifiesComponent DeallocIO r
           => Entity -> DeallocIO -> Eff r ()
setDealloc k d = modify $ IM.insert k d

dealloc :: (ModifiesComponent DeallocIO r
           ,DoesIO r
           ) => Entity -> Eff r ()
dealloc k = getDealloc k >>= io . sequence_

deleteDealloc :: ModifiesComponent DeallocIO r => Entity -> Eff r ()
deleteDealloc = modify . del
  where del = IM.delete :: Entity -> IntMap DeallocIO -> IntMap DeallocIO
--------------------------------------------------------------------------------
-- System Options
--------------------------------------------------------------------------------
getOptions :: Modifies SystemOptions r => Eff r SystemOptions
getOptions = get

setOption :: Modifies SystemOptions r => SystemOption -> Eff r ()
setOption = modify . S.insert

clearOption :: Modifies SystemOptions r => SystemOption -> Eff r ()
clearOption = modify . S.delete

optionIsSet :: Modifies SystemOptions r => SystemOption -> Eff r Bool
optionIsSet o = S.member o <$> getOptions
--------------------------------------------------------------------------------
-- Destroying an entire entity and all its components
--------------------------------------------------------------------------------
destroyEntity :: Modifies SystemCommands r => Entity -> Eff r ()
destroyEntity = modify . (:) . SystemDeleteEntity
--------------------------------------------------------------------------------
-- Common Helpers
--------------------------------------------------------------------------------
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
