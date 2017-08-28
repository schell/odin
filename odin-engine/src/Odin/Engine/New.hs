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

import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar,
                                              readTVarIO, writeTVar)
import           Control.Lens                (at, (&), (.~))
import           Control.Monad.Reader        (MonadReader)
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
getWindow = asks $ odinWindow . sysUserData


getV2V2 :: MonadReader (SystemEvents t (OdinData r)) m => m V2V2Renderer
getV2V2 = asks $ odinV2V2Renderer . sysUserData


getV2V4 :: MonadReader (SystemEvents t (OdinData r)) m => m V2V4Renderer
getV2V4 = asks $ odinV2V4Renderer . sysUserData

--------------------------------------------------------------------------------
-- Working with Fonts
--------------------------------------------------------------------------------
defaultFont :: DefaultFont
defaultFont = DefaultFont $ fontDescriptor "../assets/fonts/KMKDSP__.ttf" 16


iconFont :: IconFont
iconFont = IconFont $ fontDescriptor "../assets/fonts/FontAwesome.otf" 16


getTVarFontMap
  :: (MonadIO m, MonadReader (SystemEvents t (OdinData r)) m)
  => m (TVar FontMap)
getTVarFontMap = asks $ odinFontMap . sysUserData


getFreshVar :: MonadReader (SystemEvents t (OdinData r)) m => m (TVar Word64)
getFreshVar = asks $ odinFresh . sysUserData


freshWith :: MonadIO m => TVar Word64 -> m Word64
freshWith tvFresh = do
  k <- liftIO $ readTVarIO tvFresh
  liftIO $ atomically $ modifyTVar' tvFresh succ
  return k


fresh
  :: (MonadIO m, MonadReader (SystemEvents t (OdinData r)) m)
  => m Word64
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


getDefaultFont :: MonadReader (SystemEvents t (OdinData r)) m => m DefaultFont
getDefaultFont = asks $ odinDefaultFont . sysUserData


getIconFont :: MonadReader (SystemEvents t (OdinData r)) m => m IconFont
getIconFont = asks $ odinIconFont . sysUserData


type ConcreteReflexSDL2 r = ReflexSDL2T r Spider (PerformEventT Spider (SpiderHost Global))


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
