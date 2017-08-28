{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
module Main where

import           Control.Applicative (liftA2)
import           Control.Arrow       ((&&&))
import           Control.Lens
import           Control.Monad       (void, guard)
import           Data.Default        (Default, def)
import qualified Data.Map            as M
import           Data.Map            (Map)
import           Data.Word           (Word64)
import           Gelatin.FreeType2
import           Gelatin.GL
import           Reflex.SDL2
import           System.Exit         (exitSuccess)

import           Odin.Engine.New

stepLayers :: MonadIO m => (Map Word64 Layer, [Layer]) -> [Layer] -> m (Map Word64 Layer, [Layer])
stepLayers (oldMap, _) layers = do
  -- First make a map of the current layers and a map of
  -- the layers that no longer exist.
  let newMap  = M.fromList $ map (layerUid &&& id) layers
      freeMap = M.difference oldMap newMap
  -- Free the old layers
  sequence_ $ liftIO . layerFree <$> freeMap
  -- Return the new map
  return (newMap, layers)


render :: MonadIO m => Window -> [Layer] -> m ()
render window layers = do
  -- Render the new layers in order
  glClearColor 0 0 0 1
  glClear GL_COLOR_BUFFER_BIT
  mapM_ (liftIO . layerRender) layers
  glSwapWindow window


----------------------------------------------------------------------
mainLoop :: Odin r t m => DynamicWriterT t [Layer] m () -> m ()
mainLoop guest = do
  window         <- getWindow
  (_, dynLayers) <- runDynamicWriterT guest
  let initial = (M.empty, [])
  evLayers <- snd <$$> accumM stepLayers initial (updated dynLayers)
  performEvent_ $ render window <$> evLayers


commitLayers :: OdinLayered r t m => Dynamic t [Layer] -> m ()
commitLayers = tellDyn


commitLayer :: OdinLayered r t m => Dynamic t Layer -> m ()
commitLayer = tellDyn . fmap pure


commitLayerWhen
  :: OdinLayered r t m
  => Dynamic t Bool -> Dynamic t Layer -> m ()
commitLayerWhen dIsAlive = commitLayers . zipDynWith f dIsAlive
  where f isAlive layer = layer <$ guard isAlive

ffor3 :: Applicative f => f a -> f b -> f c -> (a -> b -> c -> d) -> f d
ffor3 a b c f = f <$> a <*> b <*> c

------------------------------------------------------------------------------
-- | fmap 1 Functor deeper
infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap


------------------------------------------------------------------------------
-- | Sequential application 1 Functor deeper
infixl 4 <**>
(<**>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<**>) = liftA2 (<*>)


data TextFieldCfg t =
  TextFieldCfg { textFieldCfgSetTextEvent           :: Event t String
               , textFieldCfgSetColorEvent          :: Event t (V4 Float)
               , textFieldCfgSetFontDescriptorEvent :: Event t FontDescriptor
               , textFieldCfgSetTransformEvent      :: Event t [RenderTransform2]
               , textFieldCfgFreeEvent              :: Event t ()
               }
$(makeFields ''TextFieldCfg)


instance Reflex t => Default (TextFieldCfg t) where
  def = TextFieldCfg { textFieldCfgSetTextEvent           = never
                     , textFieldCfgSetColorEvent          = never
                     , textFieldCfgSetFontDescriptorEvent = never
                     , textFieldCfgSetTransformEvent      = never
                     , textFieldCfgFreeEvent              = never
                     }

data TextFieldNeeds = TextFieldNeeds String (V4 Float) FontDescriptor

data TextField = TextField { textFieldRenderer :: Renderer2
                           , textFieldSize     :: V2 Float
                           }

instance Default TextField where
  def = TextField mempty 0


freeTextField :: MonadIO m => TextField -> m ()
freeTextField = liftIO . fst . textFieldRenderer


renderTextField :: MonadIO m => [RenderTransform2] -> TextField -> m ()
renderTextField ts = liftIO . ($ ts) . snd . textFieldRenderer


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


stepTextField
  :: MonadIO m
  => V2V2Renderer
  -> TVar FontMap
  -> TextField
  -> TextFieldNeeds
  -> m TextField
stepTextField v2v2 tvFontMap tf needs = do
  liftIO $ putStrLn "step text field"
  -- Free the previous allocation
  freeTextField tf
  -- Compile/alloc a new one
  compileTextField v2v2 tvFontMap needs


----------------------------------------------------------------------
textField
  :: OdinLayered r t m
  => TextFieldCfg t
  -> m (Dynamic t (V2 Float))
textField cfg = do
  k         <- fresh
  tvFontMap <- getTVarFontMap
  v2v2      <- getV2V2

  dMayText <- holdDyn Nothing $ Just <$> (cfg ^. setTextEvent)
  dMayColr <- holdDyn Nothing $ Just <$> (cfg ^. setColorEvent)
  dMayFont <- holdDyn Nothing $ Just <$> (cfg ^. setFontDescriptorEvent)
  dTfrm    <- holdDyn [] (cfg ^. setTransformEvent)
  dIsAlive <- holdDyn True =<< headE (False <$ cfg ^. freeEvent)

  let evTextFieldNeeds = fmapMaybe id $ updated $
        TextFieldNeeds <$$> dMayText <**> dMayColr <**> dMayFont
  evTextField <- accumM (stepTextField v2v2 tvFontMap) def evTextFieldNeeds
  dTextField  <- holdDyn def evTextField

  commitLayerWhen dIsAlive $ (\f -> zipDynWith f dTfrm dTextField) $ \ts tf ->
    Layer k (renderTextField ts tf) (freeTextField tf)

  return $ textFieldSize <$> dTextField


guest :: forall r t m. (OdinLayered r t m) => m ()
guest = do
  DefaultFont fdesc <- getDefaultFont
  evPB              <- getPostBuild
  evMotion          <- asks sysMouseMotionEvent
  evButton          <- asks sysMouseButtonEvent
  dPos              <- holdDyn [] $
    leftmost [ ffor evMotion $ \motion ->
                 let P v = mouseMotionEventPos motion
                 in [moveV2 $ fromIntegral <$> v]
             , [move 100 100] <$ evPB
             ]
  void $ textField $ def & setTextEvent           .~ ("Hello"    <$ evPB)
                         & setColorEvent          .~ (V4 1 1 1 1 <$ evPB)
                         & setFontDescriptorEvent .~ (fdesc      <$ evPB)
                         & setTransformEvent      .~ updated dPos
                         & freeEvent              .~ (() <$ evButton)

  asks sysQuitEvent >>= performEvent_ . (liftIO exitSuccess <$)

main :: IO ()
main = runOdin () $ mainLoop guest
