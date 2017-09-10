{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
module Odin.Engine.New
  ( module Odin.Engine.New
  , TVar
  ) where


import           Control.Applicative         (liftA2)
import           Control.Arrow               ((&&&))
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar,
                                              readTVarIO, writeTVar)
import           Control.Lens                (at, (&), (.~))
import           Control.Monad               (forM_, guard)
import           Control.Monad.Reader        (MonadReader (..))
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Either  (runEitherT)
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import qualified Data.Vector.Unboxed         as V
import           Data.Word                   (Word64)
import           Gelatin.FreeType2           (Atlas (..), GlyphSize (..),
                                              allocAtlas)
import           Gelatin.SDL2
import           Reflex.SDL2
import           Reflex.SDL2.Internal        (SystemEvents (sysUserData))
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


data Shape =
    ShapeRectangle (V2 Float) Float Float
    -- ^ A rectangle with a point for the top left and scalar width and height.
  | ShapeCircle (V2 Float) Float
    -- ^ A circle with a point for the center and a radius.
  | ShapePath (Vector (V2 Float))


transformShape :: [RenderTransform2] -> Shape -> Shape
transformShape ts (ShapeRectangle p w h) =
  let mv = affine2sModelview $ extractSpatial ts
      p1 = transformV2 mv p
      V2 w1 h1 = (transformV2 mv $ p + V2 w h) - p1
  in ShapeRectangle p1 w1 h1
transformShape ts (ShapeCircle p r) =
  let mv = affine2sModelview $ extractSpatial ts
      p1 = transformV2 mv p
      r1 = abs $ distance p1 $ transformV2 mv $ p + V2 r 0
  in ShapeCircle p1 r1
transformShape ts (ShapePath vs) = ShapePath $ V.map (transformV2 mv) vs
  where mv = affine2sModelview $ extractSpatial ts


shapeHasPoint :: Shape -> V2 Float -> Bool
shapeHasPoint (ShapeRectangle (V2 tlx tly) w h) (V2 px py) =
  px >= tlx && px <= (tlx + w) && py >= tly && py <= (tly + py)
shapeHasPoint (ShapeCircle v r) p = abs (distance v p) <= r
shapeHasPoint (ShapePath vs) p = pathHasPoint vs p


shapesHavePoint :: [Shape] -> V2 Float -> Bool
shapesHavePoint bs p = any (`shapeHasPoint` p) bs


data WidgetUserMotion = UserNoMotion
                      | UserMousedIntoWidget
                      | UserUsingWidget
                      | UserMousedOutOfWidget
                      deriving (Show, Eq)


data WidgetTree = WidgetTreeBranch [RenderTransform2] [WidgetTree]
                | WidgetTreeLeaf Word64 [RenderTransform2] [Shape] Renderer2


data Widget = Widget { widgetUid       :: Word64
                     , widgetTransform :: [RenderTransform2]
                     , widgetBoundary  :: [Shape]
                     , widgetRenderer2 :: Renderer2
                     }


flattenWidgetTree :: [RenderTransform2] -> WidgetTree -> [Widget]
flattenWidgetTree parentTs (WidgetTreeLeaf uid ts bs r2) =
  [Widget uid (parentTs ++ ts) bs r2]
flattenWidgetTree parentTs (WidgetTreeBranch ts ws) =
  concatMap (flattenWidgetTree $ parentTs ++ ts) ws


widgetHasPoint :: Widget -> V2 Float -> Bool
widgetHasPoint (Widget _ ts bs _) p =
  any ((`shapeHasPoint` p) . transformShape ts) bs


widgetsHavePoint :: [Widget] -> V2 Float -> Bool
widgetsHavePoint nodes p = any (`widgetHasPoint` p) nodes


instance Monoid WidgetTree where
  mempty = WidgetTreeBranch [] []
  mappend a b = WidgetTreeBranch [] [a, b]


data OdinData r t = OdinData { odinUserData     :: r
                             , odinWindow       :: Window
                             , odinFontMap      :: TVar FontMap
                             , odinFresh        :: TVar Word64
                             , odinV2V4Renderer :: V2V4Renderer
                             , odinV2V2Renderer :: V2V2Renderer
                             , odinDefaultFont  :: DefaultFont
                             , odinIconFont     :: IconFont
                             , odin12FPSEvent   :: Event t ()
                             , odinWidgetTree   :: WidgetTree
                             }


renderWidget :: Widget -> [RenderTransform2] -> IO ()
renderWidget w = snd (widgetRenderer2 w)


freeWidget :: Widget -> IO ()
freeWidget = fst . widgetRenderer2


stepWidgets
  :: MonadIO m
  => (Map Word64 Widget, [Widget])
  -> [Widget]
  -> m (Map Word64 Widget, [Widget])
stepWidgets (oldMap, _) widgets = do
  -- First make a map of the current widgets and a map of
  -- the widgets that no longer exist.
  let newMap  = M.fromList $ map (widgetUid &&& id) widgets
      freeMap = M.difference oldMap newMap
  -- Free the old widgets
  sequence_ $ liftIO . freeWidget <$> freeMap
  -- Return the new map
  return (newMap, widgets)


type Odin r t m = (ReflexSDL2 (OdinData r t) t m, MonadIO (PushM t))
type OdinWidget r t m = (Odin r t m, MonadDynamicWriter t WidgetTree m)


getWindow :: Odin r t m => m Window
getWindow = odinWindow <$> getUserData


getWindowSizeEvent :: Odin r t m => m (Event t (V2 Int))
getWindowSizeEvent = do
  evPB   <- getPostBuild
  window <- getWindow
  v2Cint <- get $ windowSize window
  let sz = fromIntegral <$> v2Cint
      evFirstSize = sz <$ evPB
  evResized <-
    fmap fromIntegral . windowResizedEventSize <$$> getWindowResizedEvent
  return $ leftmost [evFirstSize, evResized]


getMousePositionEvent :: Odin r t m => m (Event t (V2 Float))
getMousePositionEvent = do
  evMotion <- getMouseMotionEvent
  let mousePos dat = ($ mouseMotionEventPos dat) $ \(P v) -> fromIntegral <$> v
  return $ mousePos <$> evMotion


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

-- | TODO: Change this to 'getFontMapVar'
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


initialize12FPSEvent :: Odin r t m => m a -> m a
initialize12FPSEvent guest = do
  ev12FPS <- getRecurringTimerEvent $ floor $ 1000/(12 :: Double)
  userLocal (\o -> o{odin12FPSEvent = ev12FPS}) guest


runOdin :: Reflex t => r -> ConcreteReflexSDL2 (OdinData r t) a -> IO ()
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
                      , odin12FPSEvent   = never
                      }
  host odin guest


render :: MonadIO m => Window -> [Widget] -> m ()
render window widgets = do
  liftIO $ print $ map widgetUid widgets
  -- Render the new widgets in order
  glClearColor 0 0 0 1
  glClear GL_COLOR_BUFFER_BIT
  v2Cint <- get $ windowSize window
  let V2 ww wh = fromIntegral <$> v2Cint
  glViewport 0 0 ww wh
  forM_ widgets $ \w -> liftIO (renderWidget w $ widgetTransform w)
  glSwapWindow window


----------------------------------------------------------------------
mainLoop :: Odin r t m => DynamicWriterT t WidgetTree m () -> m ()
mainLoop guest = do
  window          <- getWindow
  (_, dWidgetTree) <- runDynamicWriterT guest
  let dNodes  = flattenWidgetTree [] <$> dWidgetTree
      initial = (M.empty, [])
  evWidgets <- snd <$$> accumM stepWidgets initial (updated dNodes)
  performEvent_ $ render window <$> evWidgets



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
