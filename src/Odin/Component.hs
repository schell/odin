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
getTransforms :: Member (State (IntMap PictureTransform)) r
              => Eff r (IntMap PictureTransform)
getTransforms = get

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
--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------
setName :: ModifiesComponent Name r => Entity -> Name -> Eff r ()
setName k n = modify $ IM.insert k n
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
