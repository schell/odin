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
import           Data.Set (Set)

import           Odin.Common
import           Odin.Physics
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
--------------------------------------------------------------------------------
-- Scripts
--------------------------------------------------------------------------------
getScripts :: Modifies [Script] r => Eff r [Script]
getScripts = get

addScript :: Modifies [Script] r => ScriptStep -> Eff r ()
addScript = modify . (:) . Script

addScripts :: (Modifies [Script] r) => [Script] -> Eff r ()
addScripts = modify . (++)
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

deleteWorldObject :: Modifies OdinScene r => Entity -> Eff r ()
deleteWorldObject k = modifyWorldObjects $ IM.delete k
--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------
setName :: ModifiesComponent Name r => Entity -> Name -> Eff r ()
setName k n = modify $ IM.insert k n

deleteName :: ModifiesComponent Name r => Entity -> Eff r ()
deleteName k = modify (IM.delete k :: IntMap Name -> IntMap Name)
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
destroyEntity :: Entity -> System ()
destroyEntity k = do
  dealloc k
  deleteRenderer k
  deletePicTransform k
  deleteWorldObject k
  deleteName k
