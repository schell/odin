{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Odin.Engine.New.UI.TextField
  ( module Odin.Engine.New.UI.TextField
  , module TF
  ) where

import           Data.Default               (Default, def)
import           Gelatin.FreeType2
import           Gelatin.GL
import           Reflex.SDL2

import           Odin.Engine.New
import           Odin.Engine.New.UI.Configs as TF (TextFieldCfg,
                                                   setColorEvent,
                                                   setFontDescriptorEvent,
                                                   setTextEvent,
                                                   setTransformEvent, (.~),
                                                   (^.))


data TextFieldNeeds = TextFieldNeeds String (V4 Float) FontDescriptor


data TextField = TextField { textFieldRenderer :: Renderer2
                           , textFieldSize     :: V2 Float
                           }


instance Default TextField where
  def = TextField mempty 0


freeTextField :: TextField -> IO ()
freeTextField = fst . textFieldRenderer


renderTextField :: [RenderTransform2] -> TextField -> IO ()
renderTextField ts = ($ ts) . snd . textFieldRenderer


compileTextField
  :: MonadIO m
  => V2V2Renderer
  -> TVar FontMap
  -> TextFieldNeeds
  -> m TextField
compileTextField (V2V2Renderer backend) tvFontMap (TextFieldNeeds text colr font) =
  loadAtlasInto tvFontMap font asciiChars >>= \case
    Nothing     -> do
      liftIO $ putStrLn "Error allocating text."
      return def
    Just atlas0 -> do
      (r, sz, atlas) <- liftIO $ freetypeRenderer2 backend atlas0 colr text
      saveAtlasInto tvFontMap atlas
      return $ TextField r sz


----------------------------------------------------------------------
textField
  :: OdinWidget r t m
  => TextFieldCfg t
  -> m (Dynamic t (V2 Float))
textField cfg = do
  tvFresh   <- getFreshVar
  tvFontMap <- getTVarFontMap
  v2v2      <- getV2V2

  dTfrm     <- holdDyn [] (cfg ^. setTransformEvent)
  dMayText  <- holdDyn Nothing $ Just <$> (cfg ^. setTextEvent)
  dMayColr  <- holdDyn Nothing $ Just <$> (cfg ^. setColorEvent)
  dMayFont  <- holdDyn Nothing $ Just <$> (cfg ^. setFontDescriptorEvent)
  let dMayNeeds = forDyn3 dMayText dMayColr dMayFont $ \ma mb mc ->
                    TextFieldNeeds <$> ma <*> mb <*> mc
      evNeeds   = fmapMaybe id $ updated dMayNeeds
      mkLayer needs = do
        k  <- freshWith tvFresh
        tf <- compileTextField v2v2 tvFontMap needs
        return (tf, \ts -> [Widget k ts [] (textFieldRenderer tf) Nothing])
  evTFAndMkLayers <- performEvent $ mkLayer <$> evNeeds
  dTFAndMkLayers  <- holdDyn (def, const []) evTFAndMkLayers
  let dLayers = zipDynWith (\(_, f) ts -> f ts) dTFAndMkLayers dTfrm
  tellDyn dLayers

  return $ textFieldSize . fst <$> dTFAndMkLayers
