{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Odin.Engine.New.UI.TextField
  ( module Odin.Engine.New.UI.TextField
  , module TF
  ) where

import           Data.Word                  (Word64)
import           Gelatin.FreeType2
import           Gelatin.GL
import           Reflex.SDL2

import           Odin.Engine.New
import           Odin.Engine.New.UI.Configs as TF (TextFieldCfg, setColorEvent,
                                                   setFontDescriptorEvent,
                                                   setTextEvent,
                                                   setTransformEvent, (.~),
                                                   (^.))


data TextFieldUpdate = TextFieldUpdateTransform [RenderTransform2]
                     | TextFieldUpdateText String
                     | TextFieldUpdateFont FontDescriptor
                     | TextFieldUpdateColor (V4 Float)


data TextFieldInternal = TF { tfK         :: Word64
                            , tfTransform :: [RenderTransform2]
                            , tfRenderer  :: Renderer2
                            , tfBoundary  :: Shape
                            , tfText      :: String
                            , tfFont      :: FontDescriptor
                            , tfColor     :: V4 Float
                            }


freeTextField :: TextFieldInternal -> IO ()
freeTextField = fst . tfRenderer


renderTextField :: [RenderTransform2] -> TextFieldInternal -> IO ()
renderTextField ts = ($ ts) . snd . tfRenderer


toWidget :: TextFieldInternal -> Widget
toWidget tf = Widget { widgetUid       = tfK tf
                     , widgetTransform = tfTransform tf
                     , widgetBoundary  = [tfBoundary tf]
                     , widgetRenderer2 = tfRenderer tf
                     , widgetCursor    = Nothing
                     }


foldTextField
  :: MonadIO m
  => V2V2Renderer
  -> TVar Word64
  -> TVar FontMap
  -> TextFieldInternal
  -> TextFieldUpdate
  -> m TextFieldInternal
foldTextField v2v2 tvFresh tvFontMap tf up
  | TextFieldUpdateTransform ts <- up = return tf {tfTransform = ts}

  | TextFieldUpdateText str <- up
  , tf1 <- tf {tfText = str} = newTF v2v2 tvFresh tvFontMap tf1

  | TextFieldUpdateFont font <- up
  , tf1 <- tf {tfFont = font} = newTF v2v2 tvFresh tvFontMap tf1

  | TextFieldUpdateColor color <- up
  , tf1 <- tf {tfColor = color} = newTF v2v2 tvFresh tvFontMap tf1


newTF
  :: MonadIO m
  => V2V2Renderer
  -> TVar Word64
  -> TVar FontMap
  -> TextFieldInternal
  -> m TextFieldInternal
newTF (V2V2Renderer backend) tvFresh tvFontMap tf =
  loadAtlasInto tvFontMap (tfFont tf) (tfText tf) >>= \case
    Nothing     -> do
      liftIO $ putStrLn "Error allocating text."
      return tf
    Just atlas0 -> do
      (r, V2 w h, atlas) <- liftIO $
        freetypeRenderer2 backend atlas0 (tfColor tf) (tfText tf)
      saveAtlasInto tvFontMap atlas
      k <- liftIO $ freshWith tvFresh
      return tf { tfK        = k
                , tfRenderer = r
                , tfBoundary = ShapeRectangle (V2 0 (-h)) w h
                }


----------------------------------------------------------------------
textFieldWith
  :: OdinWidget r t m
  => FontDescriptor
  -> V4 Float
  -> String
  -> [RenderTransform2]
  -> TextFieldCfg t
  -> m ()
textFieldWith font color str ts cfg = do
  tvFresh   <- getFreshVar
  tvFontMap <- getTVarFontMap
  v2v2      <- getV2V2

  let evUpdate = leftmost [ TextFieldUpdateTransform <$> cfg ^. setTransformEvent
                          , TextFieldUpdateText      <$> cfg ^. setTextEvent
                          , TextFieldUpdateFont      <$> cfg ^. setFontDescriptorEvent
                          , TextFieldUpdateColor     <$> cfg ^. setColorEvent
                          ]
  initial <- liftIO $
    newTF v2v2 tvFresh tvFontMap TF { tfK         = 0
                                    , tfFont      = font
                                    , tfTransform = ts
                                    , tfColor     = color
                                    , tfText      = str
                                    , tfRenderer  = mempty
                                    , tfBoundary  = ShapeRectangle 0 0 0
                                    }
  dTextField <- accumM (foldTextField v2v2 tvFresh tvFontMap) initial evUpdate
  tellDyn $ pure . toWidget <$> dTextField


----------------------------------------------------------------------
textField
  :: OdinWidget r t m
  => V4 Float
  -> String
  -> [RenderTransform2]
  -> TextFieldCfg t
  -> m ()
textField color str ts cfg = do
  DefaultFont font <- getDefaultFont
  textFieldWith font color str ts cfg
