{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Odin.Engine.New.UI.Picture
  ( pictureWith
  , colorPicture
  , texturePicture
  , module Cfg
  , module Gelatin
  ) where

import           Control.Monad              (void)
import           Data.Vector.Unboxed        (Unbox)
import           Data.Word                  (Word64)
import           Gelatin.GL                 (GLuint, Picture, RenderTransform2,
                                             Renderer2, V2V2, V2V4,
                                             compilePicture, pictureBounds2)
import qualified Gelatin.GL                 as Gelatin
import           Reflex.SDL2

import           Odin.Engine.New
import           Odin.Engine.New.UI.Configs as Cfg (PictureCfg, setPictureEvent,
                                                    setTransformEvent, (^.))


data PictureInternal = PI { piK         :: Word64
                          , piTransform :: [RenderTransform2]
                          , piRenderer  :: Renderer2
                          , piBoundary  :: Shape
                          }


toWidget :: PictureInternal -> Widget
toWidget p = Widget { widgetUid       = piK p
                    , widgetTransform = piTransform p
                    , widgetRenderer2 = piRenderer p
                    , widgetBoundary  = [piBoundary p]
                    , widgetCursor    = Nothing
                    }


data PictureUpdate v = PictureUpdateTransform [RenderTransform2]
                     | PictureUpdatePicture (Picture GLuint v ())


foldPicture
  :: (MonadIO m, v ~ (V2 Float, c), Unbox c)
  => OdinRenderer v
  -> TVar Word64
  -> PictureInternal
  -> PictureUpdate v
  -> m PictureInternal
foldPicture v2vX tvFresh p up
  | PictureUpdateTransform ts <- up = return p {piTransform = ts}

  | PictureUpdatePicture pic  <- up = do
    k          <- freshWith tvFresh
    (shape, r) <- liftIO $ compilePicture v2vX $ do
      pic
      (tl, br) <- pictureBounds2 fst
      let V2 w h = br - tl
      return $ ShapeRectangle tl w h
    return p { piK        = k
             , piRenderer = r
             , piBoundary = shape
             }


pictureWith
  :: (OdinWidget r t m, v ~ (V2 Float, c), Unbox c)
  => OdinRenderer v
  -> Picture GLuint v void
  -> [RenderTransform2]
  -> PictureCfg v t
  -> m ()
pictureWith v2vX pic ts cfg = do
  tvFresh <- getFreshVar

  let evUpdate = leftmost [ PictureUpdateTransform <$> cfg ^. setTransformEvent
                          , PictureUpdatePicture   <$> cfg ^. setPictureEvent
                          ]
      emptyPI  = PI { piK = 0
                    , piRenderer = mempty
                    , piTransform = ts
                    , piBoundary = ShapeRectangle 0 0 0
                    }

  initial      <- foldPicture v2vX tvFresh emptyPI (PictureUpdatePicture $ void pic)
  dPicInternal <- accumM (foldPicture v2vX tvFresh) initial evUpdate

  tellDyn $ pure . toWidget <$> dPicInternal


colorPicture
  :: OdinWidget r t m
  => Picture GLuint V2V4 void
  -> [RenderTransform2]
  -> PictureCfg V2V4 t
  -> m ()
colorPicture pic ts cfg = do
  V2V4Renderer v2v4 <- getV2V4
  pictureWith v2v4 pic ts cfg


texturePicture
  :: OdinWidget r t m
  => Picture GLuint V2V2 void
  -> [RenderTransform2]
  -> PictureCfg V2V2 t
  -> m ()
texturePicture pic ts cfg = do
  V2V2Renderer v2v2 <- getV2V2
  pictureWith v2v2 pic ts cfg
