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
                                                    (^.))


data PictureInternal = PI { piK        :: Word64
                          , piRenderer :: Renderer2
                          , piBoundary :: Shape
                          }


toWidget :: PictureInternal -> Widget
toWidget p = Widget { widgetUid       = piK p
                    , widgetTransform = []
                    , widgetRenderer2 = piRenderer p
                    , widgetBoundary  = [piBoundary p]
                    , widgetCursor    = Nothing
                    }


newtype PictureUpdate v = PictureUpdatePicture (Picture GLuint v ())


foldPicture
  :: (MonadIO m, v ~ (V2 Float, c), Unbox c)
  => OdinRenderer v
  -> TVar Word64
  -> PictureInternal
  -> PictureUpdate v
  -> m PictureInternal
foldPicture v2vX tvFresh p up
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
  -> PictureCfg v t
  -> m ()
pictureWith v2vX pic cfg = do
  tvFresh <- getFreshVar
  evPB    <- getPostBuild
  let evUpdate = leftmost [ PictureUpdatePicture   <$> cfg ^. setPictureEvent
                          , PictureUpdatePicture (void pic) <$ evPB
                          ]
      emptyPI  = PI { piK        = 0
                    , piRenderer = mempty
                    , piBoundary = ShapeRectangle 0 0 0
                    }

  dPicInternal <- accumM (foldPicture v2vX tvFresh) emptyPI evUpdate

  tellDyn $ pure . toWidget <$> dPicInternal


colorPicture
  :: OdinWidget r t m
  => Picture GLuint V2V4 void
  -> PictureCfg V2V4 t
  -> m ()
colorPicture pic cfg = do
  V2V4Renderer v2v4 <- getV2V4
  pictureWith v2v4 pic cfg


texturePicture
  :: OdinWidget r t m
  => Picture GLuint V2V2 void
  -> PictureCfg V2V2 t
  -> m ()
texturePicture pic cfg = do
  V2V2Renderer v2v2 <- getV2V2
  pictureWith v2v2 pic cfg
