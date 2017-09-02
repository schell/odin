{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Odin.Engine.New
  ( module Odin.Engine.New
  , TVar
  ) where


import           Control.Applicative         (liftA2)
import           Control.Arrow               ((&&&))
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar,
                                              readTVarIO, writeTVar)
import           Control.Lens                (at, (&), (.~))
import           Control.Monad               (guard)
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Either  (runEitherT)
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Word                   (Word64)
import           Gelatin.FreeType2           (Atlas (..), GlyphSize (..),
                                              allocAtlas)
import           Gelatin.SDL2
import           Reflex.SDL2
import qualified SDL


type OdinRenderer v = Backend GLuint SDL.Event v (V2 Float) Float Raster


newtype V2V2Renderer = V2V2Renderer { unV2V2Renderer :: OdinRenderer V2V2 }
newtype V2V4Renderer = V2V4Renderer { unV2V4Renderer :: OdinRenderer V2V4 }


-- | Describes a particular font path and size.
data FontDescriptor = FontDescriptor { fontDescriptorPath      :: FilePath
                                     , fontDescriptorGlyphSize :: GlyphSize
                                     } deriving (Show, Eq, Ord)


newtype DefaultFont = DefaultFont FontDescriptor deriving (Show, Eq, Ord)
newtype IconFont    = IconFont    FontDescriptor deriving (Show, Eq, Ord)


type FontMap = Map FontDescriptor Atlas


data OdinData r = OdinData { odinUserData     :: r
                           , odinWindow       :: Window
                           , odinFontMap      :: TVar FontMap
                           , odinFresh        :: TVar Word64
                           , odinV2V4Renderer :: V2V4Renderer
                           , odinV2V2Renderer :: V2V2Renderer
                           , odinDefaultFont  :: DefaultFont
                           , odinIconFont     :: IconFont
                           }


data Layer = Layer { layerUid    :: Word64
                   , layerRender :: IO ()
                   , layerFree   :: IO ()
                   }

type Odin r t m = (ReflexSDL2 (OdinData r) t m, MonadIO (PushM t))
type OdinLayered r t m = (Odin r t m, MonadDynamicWriter t [Layer] m)


getWindow :: Odin r t m => m Window
getWindow = odinWindow <$> getUserData


getV2V2 :: Odin r t m => m V2V2Renderer
getV2V2 = odinV2V2Renderer <$> getUserData


getV2V4 :: Odin r t m => m V2V4Renderer
getV2V4 = odinV2V4Renderer <$> getUserData

--------------------------------------------------------------------------------
-- Working with Fonts
--------------------------------------------------------------------------------
defaultFont :: DefaultFont
defaultFont = DefaultFont $ fontDescriptor "../assets/fonts/KMKDSP__.ttf" 16


iconFont :: IconFont
iconFont = IconFont $ fontDescriptor "../assets/fonts/FontAwesome.otf" 16


getTVarFontMap :: Odin r t m => m (TVar FontMap)
getTVarFontMap = odinFontMap <$> getUserData


getFreshVar :: Odin r t m=> m (TVar Word64)
getFreshVar = odinFresh <$> getUserData


freshWith :: MonadIO m => TVar Word64 -> m Word64
freshWith tvFresh = do
  k <- liftIO $ readTVarIO tvFresh
  liftIO $ atomically $ modifyTVar' tvFresh succ
  return k


fresh :: Odin r t m => m Word64
fresh = getFreshVar >>= freshWith


loadAtlasInto
  :: MonadIO m
  => TVar FontMap
  -> FontDescriptor
  -> String
  -> m (Maybe Atlas)
loadAtlasInto tvFontMap desc@(FontDescriptor font sz) chars = do
  atlases <- liftIO $ readTVarIO tvFontMap
  case M.lookup desc atlases of
    Nothing -> liftIO (allocAtlas font sz chars) >>= \case
      Nothing    -> return Nothing
      Just atlas -> do saveAtlasInto tvFontMap atlas
                       return $ Just atlas
    Just atlas -> return $ Just atlas


saveAtlasInto
  :: MonadIO m
  => TVar FontMap
  -> Atlas
  -> m ()
saveAtlasInto tvFontMap atlas = liftIO $ do
  atlases <- readTVarIO tvFontMap
  atomically $ writeTVar tvFontMap (atlases & at (atlasDescriptor atlas) .~ Just atlas)


atlasDescriptor :: Atlas -> FontDescriptor
atlasDescriptor Atlas{..} = FontDescriptor atlasFilePath atlasGlyphSize


fontDescriptor :: FilePath -> Int -> FontDescriptor
fontDescriptor file px = FontDescriptor file $ PixelSize px px


getDefaultFont :: Odin r t m => m DefaultFont
getDefaultFont = odinDefaultFont <$> getUserData


getIconFont :: Odin r t m => m IconFont
getIconFont = odinIconFont <$> getUserData


runOdin :: r -> ConcreteReflexSDL2 (OdinData r) a -> IO ()
runOdin r guest = do
  initializeAll
  let ogl = defaultOpenGL{ glProfile = Core Debug 3 3 }
      cfg = defaultWindow{ windowOpenGL      = Just ogl
                         , windowResizable   = True
                         , windowHighDPI     = False
                         , windowInitialSize = V2 640 480
                         }
  Right (window, SDL2Backends v2v4 v2v2) <-
    runEitherT $ startupSDL2BackendsWithConfig cfg "odin-engine-new-exe"

  tvFontMap <- atomically $ newTVar M.empty
  tvFresh   <- atomically $ newTVar 0
  let odin = OdinData { odinUserData     = r
                      , odinWindow       = window
                      , odinFontMap      = tvFontMap
                      , odinFresh        = tvFresh
                      , odinV2V4Renderer = V2V4Renderer v2v4
                      , odinV2V2Renderer = V2V2Renderer v2v2
                      , odinDefaultFont  = defaultFont
                      , odinIconFont     = iconFont
                      }
  host odin guest


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
  liftIO $ print $ map layerUid layers
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


forDyn2 :: Reflex t => Dynamic t a -> Dynamic t b -> (a -> b -> c) -> Dynamic t c
forDyn2 da db f = zipDynWith f da db


forDyn3 :: Reflex t => Dynamic t a -> Dynamic t b -> Dynamic t c -> (a -> b -> c -> d) -> Dynamic t d
forDyn3 da db dc = forDyn2 (zipDyn da db) dc . uncurry


forDyn4
  :: Reflex t
  => Dynamic t a -> Dynamic t b -> Dynamic t c -> Dynamic t d -> (a -> b -> c -> d -> e) -> Dynamic t e
forDyn4 da db dc dd = forDyn3 (zipDyn da db) dc dd . uncurry

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
