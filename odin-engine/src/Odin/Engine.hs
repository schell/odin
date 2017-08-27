{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Odin.Engine
  ( module Odin.Engine
  , module Control.Monad.Trans.NextT
  , module Gelatin.SDL2
  , MonadIO(..)
  , MouseButton(..)
  , InputMotion(..)
  , runEitherT
  , fix
  ) where

import           Control.Concurrent         (threadDelay)
import           Control.Lens
import           Control.Monad              (unless, when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Either (runEitherT)
import           Control.Monad.Trans.Reader (ReaderT (..))
import qualified Control.Monad.Trans.Reader as ReaderT
import           Data.Function              (fix)
import qualified Data.IntMap                as IM
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Word                  (Word32)
import           Foreign.C.String           (peekCString)
import           Gelatin.FreeType2          (Atlas (..), GlyphSize (..),
                                             allocAtlas)
import           Gelatin.SDL2               hiding (trace)
import           GHC.Exts                   (Constraint)
import           SDL                        hiding (Cursor, freeCursor, get,
                                             time, trace)
import           SDL.Raw.Enum               hiding (Keycode, Scancode)
import           SDL.Raw.Event              hiding (getModState)
import           SDL.Raw.Types              (Cursor)
import           System.Exit                (exitSuccess)
--------------------------------------------------------------------------------
--import           Odin.Engine.Eff.Common     as E
--import           Odin.Engine.Eff.Coroutine  as E
import           Control.Monad.Trans.NextT
import           Odin.Engine.Physics        hiding (Constraint)
import           Odin.Engine.Slots          (Slot, is, slotVar, unslot)
--------------------------------------------------------------------------------
-- Flow control
--------------------------------------------------------------------------------
-- | Function application. Applies two parameters to a function of two parameters.
with2 :: a -> b -> (a -> b -> c) -> c
with2 a b f = f a b

-- | Function application. Applies three parameters to a function of two parameters.
with3 :: a -> b -> c -> (a -> b -> c -> d) -> d
with3 a b c f = f a b c


-- | Function application. Applies four parameters to a function of two
-- parameters.
with4 :: a -> b -> c -> d -> (a -> b -> c -> d -> e) -> e
with4 a b c d f = f a b c d

-- | Function application. Applies five parameters to a function of two
-- parameters.
with5 :: a -> b -> c -> d -> e -> (a -> b -> c -> d -> e -> f) -> f
with5 a b c d e f = f a b c d e

-- | Function application. Applies six parameters to a function of two
-- parameters.
with6 :: a -> b -> c -> d -> e -> f -> (a -> b -> c -> d -> e -> f -> g) -> g
with6 a b c d e f g = g a b c d e f

-- | Convert a predicate to a partial function.
partial :: (a -> Bool) -> a -> Maybe a
partial f a = if f a then Just a else Nothing
--------------------------------------------------------------------------------
-- A specialized reader transformer.
--------------------------------------------------------------------------------
class OdinReader r m where
  ask    :: m r
  local  :: (r -> r) -> m a -> m a
  reader :: (r -> a) -> m a

instance Monad m => OdinReader r (ReaderT r m) where
  ask    = ReaderT.ask
  local  = ReaderT.local
  reader = ReaderT.reader

instance Monad m => OdinReader (Slot SystemTime) (ReaderT ReaderData m) where
  ask     = reader frameSlotSystemTime
  local f = local $ \r -> r{ frameSlotSystemTime = f $ frameSlotSystemTime r}
  reader  = reader . (. frameSlotSystemTime)

instance Monad m => OdinReader (Slot Fresh) (ReaderT ReaderData m) where
  ask     = reader frameSlotFresh
  local f = local $ \r -> r{ frameSlotFresh = f $ frameSlotFresh r}
  reader  = reader . (. frameSlotFresh)

instance Monad m => OdinReader (Slot FontMap) (ReaderT ReaderData m) where
  ask     = reader frameSlotFontMap
  local f = local $ \r -> r{ frameSlotFontMap = f $ frameSlotFontMap r}
  reader  = reader . (. frameSlotFontMap)

instance Monad m => OdinReader (Slot Ui) (ReaderT ReaderData m) where
  ask     = reader frameSlotUi
  local f = local $ \r -> r{ frameSlotUi = f $ frameSlotUi r}
  reader  = reader . (. frameSlotUi)

instance Monad m => OdinReader V2V4Renderer (ReaderT ReaderData m) where
  ask     = reader frameV2V4Renderer
  local f = local $ \r -> r{ frameV2V4Renderer = f $ frameV2V4Renderer r}
  reader  = reader . (. frameV2V4Renderer)

instance Monad m => OdinReader V2V2Renderer (ReaderT ReaderData m) where
  ask     = reader frameV2V2Renderer
  local f = local $ \r -> r{ frameV2V2Renderer = f $ frameV2V2Renderer r}
  reader  = reader . (. frameV2V2Renderer)

instance Monad m => OdinReader DefaultFont (ReaderT ReaderData m) where
  ask     = reader frameDefaultFont
  local f = local $ \r -> r{ frameDefaultFont = f $ frameDefaultFont r}
  reader  = reader . (. frameDefaultFont)

instance Monad m => OdinReader IconFont (ReaderT ReaderData m) where
  ask     = reader frameIconFont
  local f = local $ \r -> r{ frameIconFont = f $ frameIconFont r}
  reader  = reader . (. frameIconFont)
--------------------------------------------------------------------------------
-- Rendering pictures
--------------------------------------------------------------------------------
type OdinRenderer v = Backend GLuint SDL.Event v (V2 Float) Float Raster

newtype V2V2Renderer = V2V2Renderer { unV2V2Renderer :: OdinRenderer V2V2 }
newtype V2V4Renderer = V2V4Renderer { unV2V4Renderer :: OdinRenderer V2V4 }

type ReadsRendererV2V2 m = (OdinReader V2V2Renderer m, Monad m)
type ReadsRendererV2V4 m = (OdinReader V2V4Renderer m, Monad m)
type ReadsRenderers m = (ReadsRendererV2V2 m, ReadsRendererV2V4 m)

data Painting = Painting { paintingBounds   :: (V2 Float, V2 Float)
                         , paintingRenderer :: Renderer2
                         }

paintingSize :: Painting -> V2 Float
paintingSize p = let (tl,br) = paintingBounds p in br - tl

paintingOrigin :: Painting -> V2 Float
paintingOrigin = fst . paintingBounds

paintingCenter :: Painting -> V2 Float
paintingCenter p = let (tl,br) = paintingBounds p in tl + (br - tl)/2

instance Monoid Painting where
  mempty = Painting (0,0) mempty
  mappend (Painting abb a) (Painting bbb b) = Painting cbb c
    where cbb = listToBox [fst abb, snd abb, fst bbb, snd bbb]
          c   = a `mappend` b

newtype Painter a m = Painter { unPainter :: a -> m Painting }

allocColorPicRenderer :: (ReadsRendererV2V4 m, MonadIO m)
                      => ColorPicture a -> m (a, Renderer2)
allocColorPicRenderer pic = do
  V2V4Renderer b <- ask
  liftIO $ compilePicture b pic

allocTexturePicRenderer :: (ReadsRendererV2V2 m, MonadIO m)
                        => TexturePicture a -> m (a, Renderer2)
allocTexturePicRenderer pic = do
  V2V2Renderer b <- ask
  liftIO $ compilePicture b pic

backOpsV2V2 :: ReadsRendererV2V2 m => m (BackendOps GLuint Event)
backOpsV2V2 = do
  V2V2Renderer b <- ask
  return $ backendOps b

backOpsV2V4 :: ReadsRendererV2V4 m => m (BackendOps GLuint Event)
backOpsV2V4 = do
  V2V4Renderer b <- ask
  return $ backendOps b

v2v4Backend :: ReadsRendererV2V4 m => m (OdinRenderer V2V4)
v2v4Backend = unV2V4Renderer <$> ask

v2v2Backend :: ReadsRendererV2V2 m => m (OdinRenderer V2V2)
v2v2Backend = unV2V2Renderer <$> ask

getWindowSize :: (MonadIO m, ReadsRendererV2V2 m) => m (V2 Float)
getWindowSize = do
  ops <- backOpsV2V2
  fmap fromIntegral <$> liftIO (backendOpGetWindowSize ops)

getFramebufferSize :: (MonadIO m, ReadsRendererV2V2 m) => m (V2 Float)
getFramebufferSize = do
  ops <- backOpsV2V2
  fmap fromIntegral <$> liftIO (backendOpGetFramebufferSize ops)

getResolutionScale :: (MonadIO m, ReadsRendererV2V2 m) => m (V2 Float)
getResolutionScale = do
  wsz  <- getWindowSize
  fbsz <- getFramebufferSize
  return $ fbsz/wsz
--------------------------------------------------------------------------------
-- The ReaderT design pattern for mutation.
-- I liked this enough that I'm trying it out:
-- https://www.fpcomplete.com/blog/2017/06/readert-design-pattern
--------------------------------------------------------------------------------
type Mutate a m = (OdinReader (Slot a) m, MonadIO m)

type family Mutates m r :: Constraint where
            Mutates (t ': c) r = (Mutate t r, Mutates c r)
            Mutates '[] r = ()

type family Reads m r :: Constraint where
            Reads (t ': c) r = (OdinReader t r, Reads c r)
            Reads '[] r = ()

get :: Mutate a m => m a
get = ask >>= unslot

put :: Mutate a m => a -> m ()
put a = ask >>= (`is` a)

modify :: Mutate a m => (a -> a) -> m ()
modify f = get >>= put . f
--------------------------------------------------------------------------------
-- Freshness
--------------------------------------------------------------------------------
newtype Fresh = Fresh { unFresh :: Int } deriving (Show, Eq, Ord, Num, Enum)

fresh :: Mutate Fresh m => m Fresh
fresh = do
  k <- get
  put $ succ k
  return k
--------------------------------------------------------------------------------
-- User Interface
--------------------------------------------------------------------------------
data UiItem = UiItemNone | UiItemBlocked | UiItemJust Int
            deriving (Show, Eq)

data Ui = Ui { uiActiveId      :: UiItem
             , uiMousePos      :: V2 Int
             , uiMousePosRel   :: V2 Int
             , uiMouseBtn      :: MouseButton -> Bool
             , uiTextEvent     :: String
             , uiDroppedFiles  :: [FilePath]
             , uiKeyMod        :: KeyModifier
             , uiQueryScan     :: Scancode -> InputMotion -> Bool -> Bool
             , uiQueryKey      :: Keycode -> InputMotion -> Bool -> Bool
             , uiQueryMouseBtn :: MouseButton -> InputMotion -> Int -> Bool
             , uiSystemCursor  :: Word32
             , uiSavedCursor   :: Maybe (Word32, Cursor)
             }

emptyUi :: Ui
emptyUi = Ui { uiActiveId     = UiItemNone
             , uiMousePos     = V2 (-1) (-1)
             , uiMousePosRel  = 0
             , uiMouseBtn     = const False
             , uiTextEvent    = []
             , uiDroppedFiles = []
             , uiKeyMod = KeyModifier False False False False False False
                                      False False False False False
             , uiQueryScan     = \_ _ _ -> False
             , uiQueryKey      = \_ _ _ -> False
             , uiQueryMouseBtn = \_ _ _ -> False
             , uiSystemCursor  = SDL_SYSTEM_CURSOR_ARROW
             , uiSavedCursor   = Nothing
             }

queryKeycodeEvent
  :: Mutate Ui m
  => Keycode
  -- ^ The key code to query for
  -> InputMotion
  -- ^ Pressed or Released
  -> Bool
  -- ^ True if querying for a repeating key press from the user
  -- holding the key down.
  -> m Bool
queryKeycodeEvent k im rep = do
  q <- uiQueryKey <$> get
  return $ q k im rep

queryScancodeEvent
  :: Mutate Ui m
  => Scancode
  -- ^ The key code to query for.
  -> InputMotion
  -- ^ Pressed or Released.
  -> Bool
  -- ^ True if querying for a repeating key press from the user
  -- holding the key down.
  -> m Bool
queryScancodeEvent k im rep = do
  q <- uiQueryScan <$> get
  return $ q k im rep

queryMouseButtonEvent
  :: Mutate Ui m
  => MouseButton
  -- ^ The mobutton to query for. <$> get
  -> InputMotion
  -- ^ Pressed or Released.
  -> Int
  -- ^ The amount of clicks. 1 for a single-click, 2 for a
  -- double-click, etc.
  -> m Bool
queryMouseButtonEvent k im clk = do
  q <- uiQueryMouseBtn <$> get
  return $ q k im clk

queryMouseButton :: Mutate Ui m => MouseButton -> m Bool
queryMouseButton k = do
  f <- uiMouseBtn <$> get
  return $ f k

-- | Run some computation with a modified implicit Ui. Restore the previous Ui
-- while updating some key fields, maintaining any new active ids and system
-- cursor.
withLocalUi :: Mutate Ui m => Bool -> (Ui -> Ui) -> m a -> m a
withLocalUi blocked f g = do
  ui <- get
  put $ f ui
  when blocked setUIBlocked
  a       <- g
  childUI <- get
  -- restore the outer ui with the possibly active id
  put $
    if uiActiveId childUI /= UiItemBlocked
    then ui{ uiActiveId     = uiActiveId childUI
           , uiSystemCursor = uiSystemCursor childUI
           }
    else ui
  return a

getCanBeActive :: Mutate Ui m => m Bool
getCanBeActive = f . uiActiveId <$> get
  where f UiItemBlocked  = False
        f (UiItemJust _) = False
        f _              = True

setUIActiveId :: Mutate Ui m => UiItem -> m ()
setUIActiveId item = do
  ui <- get
  put ui{ uiActiveId = item }

setUIActive :: Mutate Ui m => Int -> m ()
setUIActive = setUIActiveId . UiItemJust

setUIBlocked :: Mutate Ui m => m ()
setUIBlocked = setUIActiveId UiItemBlocked

setSystemCursor :: Mutate Ui m => Word32 -> m ()
setSystemCursor n = do
  ui <- get
  put ui{ uiSystemCursor = n }

getMousePosition :: Mutate Ui m => m (V2 Int)
getMousePosition = uiMousePos <$> get

-- | Poll for new events from the backend, then fold them up into a Ui state
tickUIPrepare :: (Mutate Ui m, ReadsRendererV2V4 m, MonadIO m) => m ()
tickUIPrepare = do
  ops    <- backOpsV2V4
  evs    <- map eventPayload <$> liftIO (backendOpGetEvents ops)
  lastUi <- get
  if any isQuitKeyEvent evs
    then liftIO exitSuccess
    else do
      modstate   <- liftIO getModState
      P mousepos <- liftIO getAbsoluteMouseLocation
      P mouserel <- liftIO getRelativeMouseLocation
      mousebtnf  <- liftIO getMouseButtons
      let keycodes :: Map Keycode (InputMotion, Bool)
          keycodes  = M.fromList $ concatMap keycode evs
          scancodes :: Map Scancode (InputMotion, Bool)
          scancodes = M.fromList $ concatMap scancode evs
          mousebtns :: Map MouseButton (InputMotion, Int)
          mousebtns = M.fromList $ concatMap mousebtn evs
          textevs  = concatMap textev evs
          drpfiles = concatMap drpfile evs
      files <- mapM (liftIO . peekCString) drpfiles
      put lastUi { uiActiveId      = UiItemNone
                 , uiMousePos      = fromIntegral <$> mousepos
                 , uiMousePosRel   = fromIntegral <$> mouserel
                 , uiMouseBtn      = mousebtnf
                 , uiTextEvent     = textevs
                 , uiDroppedFiles  = files
                 , uiKeyMod        = modstate
                 , uiQueryScan     = querys scancodes
                 , uiQueryKey      = queryk keycodes
                 , uiQueryMouseBtn = querymbtn mousebtns
                 , uiSystemCursor  = SDL_SYSTEM_CURSOR_ARROW
                 }
  where isQuitKeyEvent (KeyboardEvent (KeyboardEventData _ _ _ k)) = isQuit k
        isQuitKeyEvent _                                           = False
        keycode (KeyboardEvent (KeyboardEventData _ im rep (Keysym _ k _))) =
          [(k, (im,rep))]
        keycode _ = []
        scancode (KeyboardEvent (KeyboardEventData _ im rep (Keysym s _ _))) =
          [(s, (im,rep))]
        scancode _ = []
        mousebtn (MouseButtonEvent (MouseButtonEventData _ im _ btn clk _)) =
          [(btn, (im,fromIntegral clk))]
        mousebtn _ = []
        textev (TextInputEvent (TextInputEventData _ t)) = T.unpack t
        textev _                                         = []
        drpfile (DropEvent (DropEventData file)) = [file]
        drpfile _                                = []
        queryk m k im rep = case M.lookup k m of
          Nothing          -> False
          Just (im0, rep0) -> im == im0 && rep == rep0
        querys m s im rep = case M.lookup s m of
          Nothing          -> False
          Just (im0, rep0) -> im == im0 && rep == rep0
        querymbtn m k im clk = case M.lookup k m of
          Nothing          -> False
          Just (im0, clk0) -> im == im0 && clk == clk0

-- | Finishes up some book keeping about the Ui state.
tickUIFinish :: (Mutate Ui m, MonadIO m) => m ()
tickUIFinish = do
  -- Update the cursor
  ui :: Ui <- get
  let msaved = uiSavedCursor ui
      cursor = uiSystemCursor ui
  let mkNewCursor new = do
        newcursor <- liftIO $ do
          newcursor <- createSystemCursor new
          setCursor newcursor
          return newcursor
        put ui{ uiSavedCursor = Just (new, newcursor) }
  case msaved of
    Nothing             -> unless (cursor == SDL_SYSTEM_CURSOR_ARROW) $
      mkNewCursor cursor
    Just (oldname, old) -> unless (cursor == oldname) $ do
      liftIO $ freeCursor old
      mkNewCursor cursor
--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------
data SystemTime = SystemTime
  { timeLast  :: Word32
  -- ^ The number of milliseconds from initialization to the last
  -- time the system was ticked.
  , timeDelta :: Word32
  -- ^ The difference between timeLast and the timeLast before it
  , timeLeft  :: Word32
  -- ^ The number of milliseconds left unconsumed by the last
  -- physics tick
  } deriving (Show, Eq)

readTimeDeltaMillis :: (Mutate SystemTime m, Num i) => m i
readTimeDeltaMillis = fromIntegral . timeDelta <$> get

readTimeDeltaSeconds :: (Mutate SystemTime m, Fractional f) => m f
readTimeDeltaSeconds = (/1000) . fromIntegral . timeDelta <$> get

withTiming :: (Mutate SystemTime m, Fractional f) => m a -> m (f, a)
withTiming f = do
  t0 <- readTimeDeltaSeconds
  a  <- f
  t1 <- readTimeDeltaSeconds
  return (t1 - t0, a)

newTime :: MonadIO m => m SystemTime
newTime = do
  t <- liftIO ticks
  let tt = SystemTime { timeLast  = t
                      , timeDelta = 0
                      , timeLeft  = 0
                      }
  return tt

tickTime :: (Mutate SystemTime m, MonadIO m) => m ()
tickTime = do
  SystemTime lastT _ left <- get
  t <- liftIO ticks
  put SystemTime{ timeLast  = t
                , timeDelta = t - lastT
                , timeLeft  = left
                }
--------------------------------------------------------------------------------
-- Physics
--------------------------------------------------------------------------------
setBody :: OdinScene -> Int -> Body -> OdinScene
setBody scene k b =
  scene & scWorld.worldObjs %~ IM.insert k (odinBodyToWorldObj b)
--------------------------------------------------------------------------------
-- Working with Fonts
--------------------------------------------------------------------------------
-- | Describes a particular font path and size.
data FontDescriptor = FontDescriptor { fontDescriptorPath      :: FilePath
                                     , fontDescriptorGlyphSize :: GlyphSize
                                     } deriving (Show, Eq, Ord)

newtype DefaultFont = DefaultFont FontDescriptor deriving (Show, Eq, Ord)
newtype IconFont    = IconFont    FontDescriptor deriving (Show, Eq, Ord)

type FontMap = Map FontDescriptor Atlas

loadAtlas
  :: Mutate FontMap m
  => FontDescriptor
  -> String
  -> m (Maybe Atlas)
loadAtlas desc@(FontDescriptor font sz) chars = do
  atlases <- get
  case M.lookup desc atlases of
    Nothing -> liftIO (allocAtlas font sz chars) >>= \case
      Nothing    -> return Nothing
      Just atlas -> do put $ M.insert desc atlas atlases
                       return $ Just atlas
    Just atlas -> return $ Just atlas

saveAtlas :: Mutate FontMap m => Atlas -> m ()
saveAtlas atlas = do
  atlases :: FontMap <- get
  put (atlases & at (atlasDescriptor atlas) .~ Just atlas)

atlasDescriptor :: Atlas -> FontDescriptor
atlasDescriptor Atlas{..} = FontDescriptor atlasFilePath atlasGlyphSize

fontDescriptor :: FilePath -> Int -> FontDescriptor
fontDescriptor file px = FontDescriptor file $ PixelSize px px

--getFontPath :: MonadIO m => String -> m FilePath
--getFontPath fontname =
--  (</> "assets" </> "fonts" </> fontname) <$> io getCurrentDirectory

readDefaultFontDescriptor :: OdinReader DefaultFont m => m DefaultFont
readDefaultFontDescriptor = ask

readIconFontDescriptor :: OdinReader IconFont m => m IconFont
readIconFontDescriptor = ask
--------------------------------------------------------------------------------
-- Sytem Control Stuff
--------------------------------------------------------------------------------
isQuit :: Keysym -> Bool
isQuit (Keysym (Scancode 20) (Keycode 113) m) = any ($ m)
    [ keyModifierLeftCtrl
    , keyModifierRightCtrl
    , keyModifierLeftGUI
    , keyModifierRightGUI
    ]
isQuit _ = False

tickPhysics :: Mutate SystemTime m => OdinScene -> m OdinScene
tickPhysics scene = do
  -- Time is in milliseconds
  SystemTime _ dt t0 <- get
  let tt = dt + t0
      -- one physics step should be 0.01
      n = floor (fromIntegral tt / 10 :: Double)
      t1 = tt - (fromIntegral n * 10)
  modify $ \t -> t{ timeLeft = t1 }
  -- Step the scene
  return scene{ _scWorld = runWorldOver 0.01 scene n }
--------------------------------------------------------------------------------
-- Running
--------------------------------------------------------------------------------
-- | A type to hold all of our mutable data.
data ReaderData = ReaderData { frameSlotSystemTime :: Slot SystemTime
                             , frameSlotFresh      :: Slot Fresh
                             , frameSlotFontMap    :: Slot FontMap
                             , frameSlotUi         :: Slot Ui
                             , frameV2V4Renderer   :: V2V4Renderer
                             , frameV2V2Renderer   :: V2V2Renderer
                             , frameDefaultFont    :: DefaultFont
                             , frameIconFont       :: IconFont
                             }

prepareFrame
  :: ( Mutates '[SystemTime, Ui] m
     , OdinReader V2V4Renderer m
     )
  => m ()
prepareFrame = do
  liftIO $ glClearColor 0.5 0.5 0.5 1
  V2V4Renderer backend <- ask
  liftIO $ backendOpClearWindow $ backendOps backend
  tickTime
  tickUIPrepare

presentFrame :: (Mutate Ui m, OdinReader V2V4Renderer m) => m ()
presentFrame = do
  tickUIFinish
  V2V4Renderer backend <- ask
  liftIO $ backendOpUpdateWindow $ backendOps backend

-- | Open an SDL2 window and initialize and return the backend renderers.
startupWindow
  :: V2 Int
  -- ^ Width and height of the window
  -> String
  -- ^ Title of the window
  -> IO (Either String SDL2Backends)
startupWindow (V2 w h) title = runEitherT $ startupSDL2Backends w h title True
--------------------------------------------------------------------------------
-- The Odin stack
--------------------------------------------------------------------------------
-- | A monad stack for Odin programs.
type OdinT m = NextT (ReaderT ReaderData m)

-- | Enters the given OdinT computation and loops it until it ends, displaying
-- each frame in the given backend.
runOdinT
  :: MonadIO m
  => SDL2Backends
  -- ^ The sdl2 rendering backends
  -> DefaultFont
  -- ^ A default font descriptor
  -> IconFont
  -- ^ An icon font descriptor
  -> ReaderT ReaderData m ()
  -- ^ A computation to run after each frame, like cleanup, value persistance etc
  -> OdinT m a
  -- ^ The game continuation to run
  -> m a
runOdinT (SDL2Backends v2v4 v2v2) defont ifont eachFrame app = do
  systemTimeVar <- slotVar =<< newTime
  freshVar      <- slotVar 0
  fontMapVar    <- slotVar mempty
  uiVar         <- slotVar emptyUi
  let readerData = ReaderData { frameSlotSystemTime = systemTimeVar
                              , frameSlotFresh      = freshVar
                              , frameSlotFontMap    = fontMapVar
                              , frameSlotUi         = uiVar
                              , frameV2V4Renderer   = V2V4Renderer v2v4
                              , frameV2V2Renderer   = V2V2Renderer v2v2
                              , frameDefaultFont    = defont
                              , frameIconFont       = ifont
                              }
  flip runReaderT readerData $ ($ app) $ fix $ \loop frame -> do
    prepareFrame
    status <- runNextT frame
    presentFrame
    case status of
      Left a          -> return a
      Right nextFrame -> do
        eachFrame
        liftIO $ threadDelay 10
        loop nextFrame
