{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
module Odin.Engine
  ( module E
  , module Odin.Engine
  , module Gelatin.SDL2
  , MouseButton(..)
  , InputMotion(..)
  , runEitherT
  , fix
  ) where

import           Control.Concurrent            (threadDelay)
import           Control.Lens
import           Control.Monad                 (unless)
import           Control.Monad.Freer.Coroutine as E
import           Control.Monad.Freer.Fresh     as E
import           Control.Monad.Freer.Reader    as E
import           Control.Monad.Freer.State     as E
import           Control.Monad.Trans.Either    (runEitherT)
import           Data.Function                 (fix)
import qualified Data.IntMap                   as IM
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Data.Word                     (Word32)
import           Foreign.C.String              (peekCString)
import           Gelatin.FreeType2             (Atlas (..), GlyphSize (..),
                                                allocAtlas)
import           Gelatin.SDL2                  hiding (trace)
import           SDL                           hiding (Cursor, freeCursor, get,
                                                time, trace)
import           SDL.Raw.Enum                  hiding (Keycode, Scancode)
import           SDL.Raw.Event                 hiding (getModState)
import           SDL.Raw.Types                 (Cursor)
import           System.Exit                   (exitSuccess)
--------------------------------------------------------------------------------
import           Odin.Engine.Eff.Common        as E
import           Odin.Engine.Eff.Coroutine     as E
import           Odin.Engine.Physics
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
-- Rendering pictures
--------------------------------------------------------------------------------
type OdinRenderer v = Backend GLuint SDL.Event v (V2 Float) Float Raster

newtype V2V2Renderer = V2V2Renderer { unV2V2Renderer :: OdinRenderer V2V2 }
newtype V2V4Renderer = V2V4Renderer { unV2V4Renderer :: OdinRenderer V2V4 }

type ReadsRendererV2V2 = Member (Reader V2V2Renderer)
type ReadsRendererV2V4 = Member (Reader V2V4Renderer)
type ReadsRenderers r = (ReadsRendererV2V2 r, ReadsRendererV2V4 r)

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

newtype Painter a r = Painter { unPainter :: a -> Eff r Painting }

allocColorPicRenderer :: (ReadsRendererV2V4 r, Member IO r)
                      => ColorPicture a -> Eff r (a, Renderer2)
allocColorPicRenderer pic = do
  V2V4Renderer b <- ask
  io $ compilePicture b pic

allocTexturePicRenderer :: (ReadsRendererV2V2 r, Member IO r)
                        => TexturePicture a -> Eff r (a, Renderer2)
allocTexturePicRenderer pic = do
  V2V2Renderer b <- ask
  io $ compilePicture b pic

backOpsV2V2 :: ReadsRendererV2V2 r => Eff r (BackendOps GLuint Event)
backOpsV2V2 = do
  V2V2Renderer b <- ask
  return $ backendOps b

backOpsV2V4 :: ReadsRendererV2V4 r => Eff r (BackendOps GLuint Event)
backOpsV2V4 = do
  V2V4Renderer b <- ask
  return $ backendOps b

v2v4Backend :: ReadsRendererV2V4 r => Eff r (OdinRenderer V2V4)
v2v4Backend = unV2V4Renderer <$> ask

v2v2Backend :: ReadsRendererV2V2 r => Eff r (OdinRenderer V2V2)
v2v2Backend = unV2V2Renderer <$> ask

getWindowSize :: (Member IO r, ReadsRendererV2V2 r) => Eff r (V2 Float)
getWindowSize = do
  ops <- backOpsV2V2
  fmap fromIntegral <$> io (backendOpGetWindowSize ops)

getFramebufferSize :: (Member IO r, ReadsRendererV2V2 r) => Eff r (V2 Float)
getFramebufferSize = do
  ops <- backOpsV2V2
  fmap fromIntegral <$> io (backendOpGetFramebufferSize ops)

getResolutionScale :: (Member IO r, ReadsRendererV2V2 r) => Eff r (V2 Float)
getResolutionScale = do
  wsz  <- getWindowSize
  fbsz <- getFramebufferSize
  return $ fbsz/wsz
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

type AltersUI = Member (State Ui)

queryKeycodeEvent
  :: Member (State Ui) r
  => Keycode
  -- ^ The key code to query for
  -> InputMotion
  -- ^ Pressed or Released
  -> Bool
  -- ^ True if querying for a repeating key press from the user
  -- holding the key down.
  -> Eff r Bool
queryKeycodeEvent k im rep = do
  q <- uiQueryKey <$> get
  return $ q k im rep

queryScancodeEvent
  :: Member (State Ui) r
  => Scancode
  -- ^ The key code to query for.
  -> InputMotion
  -- ^ Pressed or Released.
  -> Bool
  -- ^ True if querying for a repeating key press from the user
  -- holding the key down.
  -> Eff r Bool
queryScancodeEvent k im rep = do
  q <- uiQueryScan <$> get
  return $ q k im rep

queryMouseButtonEvent
  :: Member (State Ui) r
  => MouseButton
  -- ^ The mobutton to query for. <$> get
  -> InputMotion
  -- ^ Pressed or Released.
  -> Int
  -- ^ The amount of clicks. 1 for a single-click, 2 for a
  -- double-click, etc.
  -> Eff r Bool
queryMouseButtonEvent k im clk = do
  q <- uiQueryMouseBtn <$> get
  return $ q k im clk

queryMouseButton :: Member (State Ui) r => MouseButton -> Eff r Bool
queryMouseButton k = do
  f <- uiMouseBtn <$> get
  return $ f k

uiLocal :: Member (State Ui) r => (Ui -> Ui) -> Eff r a -> Eff r (Ui, a)
uiLocal f m = do
  ui0 <- get
  put $ f ui0
  a   <- m
  ui1 <- get
  put ui0
  return (ui1, a)

getCanBeActive :: Member (State Ui) r => Eff r Bool
getCanBeActive = f . uiActiveId <$> get
  where f UiItemBlocked  = False
        f (UiItemJust _) = False
        f _              = True

setUIActiveId :: Member (State Ui) r => UiItem -> Eff r ()
setUIActiveId item = do
  ui <- get
  put ui{ uiActiveId = item }

setUIActive :: Member (State Ui) r => Int -> Eff r ()
setUIActive = setUIActiveId . UiItemJust

setUIBlocked :: Member (State Ui) r => Eff r ()
setUIBlocked = setUIActiveId UiItemBlocked

setSystemCursor :: Member (State Ui) r => Word32 -> Eff r ()
setSystemCursor n = do
  ui <- get
  put ui{ uiSystemCursor = n }

getMousePosition :: Member (State Ui) r => Eff r (V2 Int)
getMousePosition = uiMousePos <$> get

-- | Poll for new events from the backend, then fold them up into a Ui state
tickUIPrepare :: (Member (State Ui) r, ReadsRendererV2V2 r, Member IO r) => Eff r ()
tickUIPrepare = do
  ops    <- backOpsV2V2
  evs    <- map eventPayload <$> io (backendOpGetEvents ops)
  lastUi <- get
  if any isQuitKeyEvent evs
    then io exitSuccess
    else do
      modstate   <- io getModState
      P mousepos <- io getAbsoluteMouseLocation
      P mouserel <- io getRelativeMouseLocation
      mousebtnf  <- io getMouseButtons
      let keycodes :: Map Keycode (InputMotion, Bool)
          keycodes  = M.fromList $ concatMap keycode evs
          scancodes :: Map Scancode (InputMotion, Bool)
          scancodes = M.fromList $ concatMap scancode evs
          mousebtns :: Map MouseButton (InputMotion, Int)
          mousebtns = M.fromList $ concatMap mousebtn evs
          textevs  = concatMap textev evs
          drpfiles = concatMap drpfile evs
      files <- mapM (io . peekCString) drpfiles
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
tickUIFinish :: (Member (State Ui) r, Member IO r) => Eff r ()
tickUIFinish = do
  -- Update the cursor
  ui :: Ui <- get
  let msaved = uiSavedCursor ui
      cursor = uiSystemCursor ui
  let mkNewCursor new = do
        newcursor <- io $ do
          newcursor <- createSystemCursor new
          setCursor newcursor
          return newcursor
        put ui{ uiSavedCursor = Just (new, newcursor) }
  case msaved of
    Nothing             -> unless (cursor == SDL_SYSTEM_CURSOR_ARROW) $
      mkNewCursor cursor
    Just (oldname, old) -> unless (cursor == oldname) $ do
      io $ freeCursor old
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

type AltersTime = Member (State SystemTime)

readTimeDeltaMillis :: (Member (State SystemTime) r, Num i) => Eff r i
readTimeDeltaMillis = fromIntegral . timeDelta <$> get

readTimeDeltaSeconds :: (Member (State SystemTime) r, Fractional f) => Eff r f
readTimeDeltaSeconds = (/1000) . fromIntegral . timeDelta <$> get

withTiming :: (Member (State SystemTime) r, Fractional f) => Eff r a -> Eff r (f, a)
withTiming f = do
  t0 <- readTimeDeltaSeconds
  a  <- f
  t1 <- readTimeDeltaSeconds
  return (t1 - t0, a)

newTime :: Member IO r => Eff r SystemTime
newTime = do
  t <- io ticks
  let tt = SystemTime { timeLast  = t
                      , timeDelta = 0
                      , timeLeft  = 0
                      }
  return tt

tickTime :: (Member (State SystemTime) r, Member IO r) => Eff r ()
tickTime = do
  SystemTime lastT _ left <- get
  t <- io ticks
  put SystemTime{ timeLast  = t
                , timeDelta = t - lastT
                , timeLeft  = left
                }
--------------------------------------------------------------------------------
-- Physics
--------------------------------------------------------------------------------
type AltersPhysics = Member (State OdinScene)

setBody :: Member (State OdinScene) r => Int -> Body -> Eff r ()
setBody k b = do
  scene :: OdinScene <- get
  put (scene & scWorld.worldObjs %~ IM.insert k (odinBodyToWorldObj b))
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

type AltersFontMap = Member (State FontMap)

loadAtlas
  :: Members '[IO, State FontMap] r
  => FontDescriptor
  -> String
  -> Eff r (Maybe Atlas)
loadAtlas desc@(FontDescriptor font sz) chars = do
  atlases <- get
  case M.lookup desc atlases of
    Nothing -> io (allocAtlas font sz chars) >>= \case
      Nothing    -> return Nothing
      Just atlas -> do put $ M.insert desc atlas atlases
                       return $ Just atlas
    Just atlas -> return $ Just atlas

saveAtlas :: Member (State FontMap) r => Atlas -> Eff r ()
saveAtlas atlas = do
  atlases :: FontMap <- get
  put (atlases & at (atlasDescriptor atlas) .~ Just atlas)

atlasDescriptor :: Atlas -> FontDescriptor
atlasDescriptor Atlas{..} = FontDescriptor atlasFilePath atlasGlyphSize

fontDescriptor :: FilePath -> Int -> FontDescriptor
fontDescriptor file px = FontDescriptor file $ PixelSize px px

--getFontPath :: Member IO r => String -> Eff r FilePath
--getFontPath fontname =
--  (</> "assets" </> "fonts" </> fontname) <$> io getCurrentDirectory

readDefaultFontDescriptor :: Member (Reader DefaultFont) r => Eff r DefaultFont
readDefaultFontDescriptor = ask

readIconFontDescriptor :: Member (Reader IconFont) r => Eff r IconFont
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


tickPhysics :: Members '[State OdinScene, State SystemTime] r => Eff r ()
tickPhysics = do
  -- Time is in milliseconds
  SystemTime _ dt t0 <- get
  let tt = dt + t0
      -- one physics step should be 0.01
      n = floor (fromIntegral tt / 10 :: Double)
      t1 = tt - (fromIntegral n * 10)
  modify $ \t -> t{ timeLeft = t1 }
  -- Step the scene
  scene :: OdinScene <- get
  put scene{ _scWorld = runWorldOver 0.01 scene n }
--------------------------------------------------------------------------------
-- Running
--------------------------------------------------------------------------------
type OdinFx r = Fresh
             ': Reader V2V4Renderer
             ': Reader V2V2Renderer
             ': Reader DefaultFont
             ': Reader IconFont
             ': State SystemTime
             ': State FontMap
             ': State Ui
             ': State Allocated
             ': r


type OdinFrame r = ( Member IO r
                   , Member (State SystemTime) r
                   , Member Fresh r
                   , Member (Reader V2V4Renderer) r
                   , Member (Reader V2V2Renderer) r
                   , Member (Reader DefaultFont) r
                   , Member (Reader IconFont) r
                   , Member (State FontMap) r
                   , Member (State Allocated) r
                   , Member (State Ui) r
                   )

type OdinCont r = (OdinFrame r, Member Next r)

-- | Run an OdinFx computation, stripping all Odin effects and returning the
-- resulting values.
runOdinFx
  :: OdinRenderer V2V2
  -> OdinRenderer V2V4
  -> DefaultFont
  -> IconFont
  -> Int
  -> SystemTime
  -> FontMap
  -> Ui
  -> Allocated
  -> Eff (OdinFx r) a
  -> Eff r (((((a, Int), SystemTime), FontMap), Ui), Allocated)
runOdinFx v2v2 v2v4 defont ifont k t fonts ui allocs =
    flip runState  allocs
  . flip runState  ui
  . flip runState  fonts
  . flip runState  t
  . flip runReader ifont
  . flip runReader defont
  . flip runReader (V2V2Renderer v2v2)
  . flip runReader (V2V4Renderer v2v4)
  . flip runFresh  k

prepareFrame :: OdinFrame r => Eff r ()
prepareFrame = do
  io $ glClearColor 0.5 0.5 0.5 1
  V2V4Renderer backend <- ask
  io $ backendOpClearWindow $ backendOps backend
  tickTime
  tickUIPrepare

presentFrame :: OdinFrame r => Eff r ()
presentFrame = do
  tickUIFinish
  V2V4Renderer backend <- ask
  io $ backendOpUpdateWindow $ backendOps backend

-- | Sandwich a continuation between a frame preparation and presentation,
-- running a separate computation after each presentation, until the continuation
-- ends in a result value.
loopFrameAnd :: OdinFrame r => Eff r (Status r () () a) -> Eff r () -> Eff r a
loopFrameAnd eff eachFrame = do
  prepareFrame
  status <- eff
  presentFrame
  case status of
    Done a        -> return a
    Continue () f -> do
      eachFrame
      io $ threadDelay 10
      loopFrameAnd (f ()) eachFrame

-- | Open an SDL2 window and initialize and return the backend renderers.
startupWindow
  :: V2 Int
  -- ^ Width and height of the window
  -> String
  -- ^ Title of the window
  -> IO (Either String SDL2Backends)
startupWindow (V2 w h) title = runEitherT $ startupSDL2Backends w h title True

-- | Enters the given Odin style continuation and loops it forever, displaying
-- each frame in the given backend.
runOdin
  :: forall r a. Member IO r
  => SDL2Backends
  -- ^ The sdl2 rendering backends
  -> DefaultFont
  -- ^ A default font descriptor
  -> IconFont
  -- ^ An icon font descriptor
  -> Eff (OdinFx r) ()
  -- ^ A computation to run after each frame, like cleanup, value persistance etc
  -> Eff (Next ': OdinFx r) a
  -- ^ The game continuation to run
  -> Eff r a
runOdin (SDL2Backends v2v4 v2v2) defont ifont eachFrame app = do
  t <- newTime
  let fy  :: Member IO r => Eff (OdinFx r) (Status (OdinFx r) () () a)
      fy = runC app
      eff :: Member IO r => Eff (OdinFx r) a
      eff = loopFrameAnd fy eachFrame
  fst . fst . fst . fst . fst <$>
    runOdinFx v2v2 v2v4 defont ifont 0 t mempty emptyUi AllocatedNone eff

-- | Run an Odin style continuation in an SDL2 window.
runOdinIO
  :: forall a.
     SDL2Backends
  -- ^ The sdl2 rendering backends
  -> DefaultFont
  -- ^ A default font descriptor
  -> IconFont
  -- ^ An icon font descriptor
  -> Eff (OdinFx '[IO]) ()
  -- ^ A computation to run after each frame, like cleanup, value persistance etc
  -> Eff (Next ': OdinFx '[IO]) a
  -- ^ The game continuation to run
  -> IO a
runOdinIO backends defont ifont eachFrame app =
  runM $ runOdin backends defont ifont eachFrame app
