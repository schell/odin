{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Odin.Engine.New.UI.Picture
  ( module Odin.Engine.New.UI.Picture
  , module Cfg
  ) where

import           Gelatin.GL                 (GLuint, Picture, Renderer2, V2V2,
                                             V2V4, compilePicture)
import           Reflex.SDL2

import           Odin.Engine.New
import           Odin.Engine.New.UI.Configs as Cfg (PictureCfg, setPictureEvent,
                                                    setTransformEvent, (^.))


compileColorPicture
  :: Odin r t m
  => Picture GLuint (V2 Float, V4 Float) ()
  -> m Renderer2
compileColorPicture pic = do
  V2V4Renderer v2v4 <- getV2V4
  snd <$> liftIO (compilePicture v2v4 pic)


compileTexturePicture
  :: Odin r t m
  => Picture GLuint (V2 Float, V2 Float) ()
  -> m Renderer2
compileTexturePicture pic = do
  V2V2Renderer v2v2 <- getV2V2
  snd <$> liftIO (compilePicture v2v2 pic)


picture :: OdinWidget r t m => OdinRenderer v -> PictureCfg GLuint v t -> m ()
picture v2vX cfg = do
  tvFresh <- getFreshVar
  dTfrm   <- holdDyn [] (cfg ^. setTransformEvent)
  let mkLayer pic = do
        k  <- freshWith tvFresh
        r2 <- snd <$> compilePicture v2vX pic
        return $ \ts -> [Widget k ts r2]
  evMkLayer <- performEvent $ mkLayer <$> (cfg ^. setPictureEvent)
  dMkLayer  <- holdDyn (const []) evMkLayer
  commitWidgets $ zipDynWith ($) dMkLayer dTfrm


colorPicture :: OdinWidget r t m => PictureCfg GLuint V2V4 t -> m ()
colorPicture cfg = do
  V2V4Renderer v2v4 <- getV2V4
  picture v2v4 cfg


texturePicture :: OdinWidget r t m => PictureCfg GLuint V2V2 t -> m ()
texturePicture cfg = do
  V2V2Renderer v2v2 <- getV2V2
  picture v2v2 cfg
