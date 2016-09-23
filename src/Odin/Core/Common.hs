{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Odin.Core.Common where

import           Gelatin.SDL2 hiding (E, move, scale, rotate, multiply)
import           Gelatin.FreeType2
import           SDL hiding (freeCursor, Cursor, Event, get, time)
import qualified SDL
import           SDL.Raw.Event hiding (getModState)
import           SDL.Raw.Enum hiding (Keycode, Scancode)
import           SDL.Raw.Types (Cursor)
import           Data.Monoid ((<>))
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import           Data.Vector.Unboxed (Unbox)
import           Data.Word (Word32)
import qualified Data.Text as T
import           Control.Concurrent.STM
import           Control.Monad.State.Strict as State
import           Control.Lens
import           Foreign.C.String
import           Linear as L hiding (rotate)
import           Linear.Affine (Point(..))
import           System.FilePath
import           System.Directory
import           System.Exit (exitSuccess)

import           Control.Monad.Evented as E

import           Odin.Core.Physics as OP
--------------------------------------------------------------------------------
-- Core types for building applications
--------------------------------------------------------------------------------
newtype Uid = Uid { unUid :: Int } deriving (Show, Eq, Num, Enum)

data SystemTime = SystemTime
  { _timeLast  :: Word32
  -- ^ The number of milliseconds from initialization to the last
  -- time the system was ticked.
  , _timeDelta :: Word32
  -- ^ The difference between timeLast and the timeLast before it
  , _timeLeft  :: Word32
  -- ^ The number of milliseconds left unconsumed by the last
  -- physics tick
  } deriving (Show, Eq)

type GUIRenderer = GLRenderer
newtype Painting = Painting {unPainting :: ((V2 Float, V2 Float) , GUIRenderer)}

instance Monoid Painting where
  mempty = Painting ((0,0), mempty)
  mappend (Painting (abb,a)) (Painting (bbb,b)) = Painting (cbb,c)
    where cbb = pointsBounds [fst abb, snd abb, fst bbb, snd bbb]
          c   = a `mappend` b

newtype Painter a m = Painter { unPainter :: a -> m Painting }

newtype FontDescriptor = FontDescriptor (FilePath, GlyphSize)
                       deriving (Show, Eq, Ord)

type FontMap = Map FontDescriptor Atlas

data UiItem = UiItemNone | UiItemBlocked | UiItemJust Int
            deriving (Show, Eq)

-- | All the UIState our apps will need to query each frame.
data Ui = Ui { _uiActiveId      :: UiItem
             , _uiMousePos      :: V2 Int
             , _uiMousePosRel   :: V2 Int
             , _uiMouseBtn      :: MouseButton -> Bool
             , _uiTextEvent     :: String
             , _uiDroppedFiles  :: [FilePath]
             , _uiKeyMod        :: KeyModifier
             , _uiQueryScan     :: Scancode -> InputMotion -> Bool -> Bool
             , _uiQueryKey      :: Keycode -> InputMotion -> Bool -> Bool
             , _uiQueryMouseBtn :: MouseButton -> InputMotion -> Int -> Bool
             , _uiSystemCursor  :: Word32
             , _uiSavedCursor   :: Maybe (Word32, Cursor)
             }
makeFields ''Ui

emptyUi :: Ui
emptyUi = Ui { _uiActiveId     = UiItemNone
             , _uiMousePos     = V2 (-1) (-1)
             , _uiMousePosRel  = 0
             , _uiMouseBtn     = const False
             , _uiTextEvent    = []
             , _uiDroppedFiles = []
             , _uiKeyMod = KeyModifier False False False False False False
                                       False False False False False
             , _uiQueryScan     = \_ _ _ -> False
             , _uiQueryKey      = \_ _ _ -> False
             , _uiQueryMouseBtn = \_ _ _ -> False
             , _uiSystemCursor  = SDL_SYSTEM_CURSOR_ARROW
             , _uiSavedCursor   = Nothing
             }
data Frame = Frame { _frameTime   :: SystemTime
                   , _frameNextK  :: Int
                   , _frameWindow :: Window
                   , _frameRez    :: Rez
                   , _frameFonts  :: FontMap
                   , _frameRsrcs  :: [IO ()]
                   , _frameUi     :: Ui
                   }
makeFields ''Frame

type UpdateT m = EventT (StateT Frame m)

instance Monad m => MonadState Frame (UpdateT m) where
  get = lift get
  put = lift . put
--------------------------------------------------------------------------------
-- Type Constraints and Abilities
--------------------------------------------------------------------------------
makeLenses ''SystemTime

class HasScene s a | s -> a where
  scene :: Lens' s a

type Fresh s m      = (MonadState s m, HasNextK  s Int)
type Time s m       = (MonadState s m, HasTime   s SystemTime)
type Physics s m    = (MonadState s m, HasScene  s OdinScene)
type Windowed s m   = (MonadState s m, HasWindow s Window)
type Rezed s m      = (MonadState s m, HasRez    s Rez)
type Fonts s m      = (MonadState s m, HasFonts  s FontMap)
type Resources s m  = (MonadState s m, HasRsrcs  s [IO ()])
type UIState s m    = (MonadState s m, HasUi     s Ui)

type GUI s m =
  (MonadIO m, UIState s m, Fresh s m, Fonts s m, Rezed s m, Windowed s m, Resources s m)
--------------------------------------------------------------------------------
-- Time Savers/Aliases
--------------------------------------------------------------------------------
io :: MonadIO m => IO a -> m a
io = liftIO
--------------------------------------------------------------------------------
-- Querying the UI state
--------------------------------------------------------------------------------
queryKeycodeEvent :: UIState s m
                  => Keycode
                  -- ^ The key code to query for
                  -> InputMotion
                  -- ^ Pressed or Released
                  -> Bool
                  -- ^ True if querying for a repeating key press from the user
                  -- holding the key down.
                  -> m Bool
queryKeycodeEvent k im rep = do
  q <- use (ui . queryKey)
  return $ q k im rep

queryScancodeEvent :: UIState s m
                   => Scancode
                   -- ^ The key code to query for.
                   -> InputMotion
                   -- ^ Pressed or Released.
                   -> Bool
                   -- ^ True if querying for a repeating key press from the user
                   -- holding the key down.
                   -> m Bool
queryScancodeEvent k im rep = do
  q <- use (ui . queryScan)
  return $ q k im rep

queryMouseButtonEvent :: UIState s m
                      => MouseButton
                      -- ^ The mouse button to query for.
                      -> InputMotion
                      -- ^ Pressed or Released.
                      -> Int
                      -- ^ The amount of clicks. 1 for a single-click, 2 for a
                      -- double-click, etc.
                      -> m Bool
queryMouseButtonEvent k im clk = do
  q <- use (ui . queryMouseBtn)
  return $ q k im clk

queryMouseButton :: UIState s m => MouseButton -> m Bool
queryMouseButton k = do
  f <- use (ui . mouseBtn)
  return $ f k

uiLocal :: UIState s m => (Ui -> Ui) -> m a -> m (Ui, a)
uiLocal f m = do
  ui0 <- use ui
  ui  .= f ui0
  a   <- m
  ui1 <- use ui
  ui  .= ui0
  return (ui1, a)

getCanBeActive :: UIState s m => m Bool
getCanBeActive = f <$> (use (ui . activeId))
  where f UiItemBlocked = False
        f (UiItemJust _) = False
        f _ = True

setActive :: UIState s m => Int -> m ()
setActive = (ui.activeId .=) . UiItemJust

getMousePosition :: UIState s m => m (V2 Int)
getMousePosition = use (ui . mousePos)
--------------------------------------------------------------------------------
-- Generating Unique IDs
--------------------------------------------------------------------------------
fresh :: Fresh s m => m Int
fresh = do
  k <- use nextK
  nextK += 1
  return k
--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------
readTimeDeltaSeconds :: (Time s m, Fractional f) => m f
readTimeDeltaSeconds = (/1000) . fromIntegral <$> use (time.timeDelta)

withTiming :: (Time s m, Fractional f) => m a -> m (f, a)
withTiming f = do
  t0 <- readTimeDeltaSeconds
  a  <- f
  t1 <- readTimeDeltaSeconds
  return (t1 - t0, a)

newTime :: MonadIO m => m SystemTime
newTime = do
  t <- io ticks
  let tt = SystemTime { _timeLast  = t
                      , _timeDelta = 0
                      , _timeLeft  = 0
                      }
  return tt
----------------------------------------------------------------------------------
-- Rendering Pictures
----------------------------------------------------------------------------------
allocColorPicRenderer :: (Rezed s m, MonadIO m) => ColorPictureT m a -> m GLRenderer
allocColorPicRenderer pic = do
  rz <- use rez
  (_,dat) <- runPictureT pic
  io $ compileColorPictureData rz dat

allocTexturePicRenderer ::(Rezed s m, MonadIO m) => TexturePictureT m a -> m GLRenderer
allocTexturePicRenderer pic = do
  rz <- use rez
  (_,dat) <- runPictureT pic
  io $ compileTexturePictureData rz dat
--------------------------------------------------------------------------------
-- Chaining Setters
--------------------------------------------------------------------------------
setBody :: Physics s m => Int -> Body -> m ()
setBody k b = scene.scWorld.worldObjs %= IM.insert k (odinBodyToWorldObj b)
--------------------------------------------------------------------------------
-- Working with Fonts
--------------------------------------------------------------------------------
loadAtlas :: (MonadIO m, Fonts s m) => FontDescriptor -> String
         -> m (Maybe Atlas)
loadAtlas desc@(FontDescriptor (font, sz)) chars = do
  atlases <- use fonts
  case M.lookup desc atlases of
    Nothing -> allocAtlas font sz chars >>= \case
      Nothing    -> return Nothing
      Just atlas -> do fonts .= M.insert desc atlas atlases
                       return $ Just atlas
    Just atlas -> return $ Just atlas

saveAtlas :: Fonts s m => Atlas -> m ()
saveAtlas atlas = fonts %= M.insert (atlasDescriptor atlas) atlas

atlasDescriptor :: Atlas -> FontDescriptor
atlasDescriptor Atlas{..} = FontDescriptor (atlasFilePath, atlasGlyphSize)

fontDescriptor :: FilePath -> Int -> FontDescriptor
fontDescriptor file px = FontDescriptor (file, PixelSize px px)

getFontPath :: MonadIO m => String -> m FilePath
getFontPath fontname =
  (</> "assets" </> "fonts" </> fontname) <$> (io getCurrentDirectory)
--------------------------------------------------------------------------------
-- Storing / Retreiving messages
--------------------------------------------------------------------------------
newtype Slot a = Slot { unSlot :: TVar a }

slot :: MonadIO m => a -> m (Slot a)
slot = (Slot <$>) . io . newTVarIO

unslot :: MonadIO m => Slot a -> m a
unslot = io . readTVarIO . unSlot

reslot :: MonadIO m => Slot a -> a -> m ()
reslot s a = void $ io $ atomically $ swapTVar (unSlot s) a

is :: MonadIO m => Slot a -> a -> m ()
is = reslot

fromSlot :: MonadIO m => Slot a -> (a -> b) -> m b
fromSlot s f = (f <$>) $ io $ readTVarIO $ unSlot s

fromSlotM :: MonadIO m => Slot a -> (a -> m b) -> m b
fromSlotM s f = unslot s >>= f

modifySlot :: MonadIO m => Slot a -> (a -> a) -> m ()
modifySlot s = io . atomically . modifyTVar' (unSlot s)

modifySlotM :: MonadIO m => Slot a -> (a -> m a) -> m ()
modifySlotM s f = unslot s >>= f >>= reslot s
--------------------------------------------------------------------------------
-- Look/Feel
--------------------------------------------------------------------------------
move :: Float -> Float -> RenderTransform
move x y = Spatial $ Translate $ V2 x y

moveV2 :: V2 Float -> RenderTransform
moveV2 (V2 x y) = move x y

scale :: Float -> Float -> RenderTransform
scale x y = Spatial $ Scale $ V2 x y

scaleV2 :: V2 Float -> RenderTransform
scaleV2 (V2 x y) = scale x y

rotate :: Float -> RenderTransform
rotate = Spatial . Rotate

multiply :: Float -> Float -> Float -> Float -> RenderTransform
multiply r g b a = Multiply $ V4 r g b a

multiplyV4 :: V4 Float -> RenderTransform
multiplyV4 (V4 r g b a) = multiply r g b a

redChannelReplacement :: Float -> Float -> Float -> Float -> RenderTransform
redChannelReplacement r g b a = ColorReplacement $ V4 r g b a

redChannelReplacementV4 :: V4 Float -> RenderTransform
redChannelReplacementV4 (V4 r g b a) = redChannelReplacement r g b a

paintingBounds :: Painting -> (V2 Float, V2 Float)
paintingBounds = fst . unPainting

paintingSize :: Painting -> V2 Float
paintingSize p = let (tl,br) = paintingBounds p in br - tl

paintingOrigin :: Painting -> V2 Float
paintingOrigin = fst . fst . unPainting

paintingCenter :: Painting -> V2 Float
paintingCenter p = let (tl,br) = paintingBounds p in tl + (br - tl)/2
--------------------------------------------------------------------------------
-- Window and Buffer stuff
--------------------------------------------------------------------------------
getWindowSize :: (Windowed s m, MonadIO m) => m (V2 Float)
getWindowSize = do
  win <- use window
  (fmap fromIntegral) <$> io (SDL.get $ windowSize win)

getFramebufferSize :: (MonadIO m, Rezed s m) => m (V2 Float)
getFramebufferSize = do
  Rez{..} <- use rez
  (w,h) <- io $ ctxFramebufferSize rezContext
  return $ fromIntegral <$> V2 w h

getResolutionScale :: (MonadIO m, Windowed s m, Rezed s m) => m (V2 Float)
getResolutionScale = do
  wsz  <- getWindowSize
  fbsz <- getFramebufferSize
  return $ fbsz/wsz
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

tickTime :: (Time s m, MonadIO m) => m ()
tickTime = do
  lastT <- use (time.timeLast)
  t <- io ticks
  time.timeLast .= t
  time.timeDelta .= t - lastT

-- | Poll for new events from the backend, then fold them up into a Ui state
tickUIPrepare :: (UIState s m, MonadIO m) => m ()
tickUIPrepare = do
  evs <- map eventPayload <$> io pollEvents
  lastUi <- use ui
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
      ui .= lastUi { _uiActiveId      = UiItemNone
                   , _uiMousePos      = fromIntegral <$> mousepos
                   , _uiMousePosRel   = fromIntegral <$> mouserel
                   , _uiMouseBtn      = mousebtnf
                   , _uiTextEvent     = textevs
                   , _uiDroppedFiles  = files
                   , _uiKeyMod        = modstate
                   , _uiQueryScan     = querys scancodes
                   , _uiQueryKey      = queryk keycodes
                   , _uiQueryMouseBtn = querymbtn mousebtns
                   , _uiSystemCursor  = SDL_SYSTEM_CURSOR_ARROW
                   }
  where isQuitKeyEvent (KeyboardEvent (KeyboardEventData _ _ _ k)) = isQuit k
        isQuitKeyEvent _ = False
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
        textev _ = []
        drpfile (DropEvent (DropEventData file)) = [file]
        drpfile _ = []
        queryk m k im rep = case M.lookup k m of
          Nothing -> False
          Just (im0, rep0) -> im == im0 && rep == rep0
        querys m s im rep = case M.lookup s m of
          Nothing -> False
          Just (im0, rep0) -> im == im0 && rep == rep0
        querymbtn m k im clk = case M.lookup k m of
          Nothing -> False
          Just (im0, clk0) -> im == im0 && clk == clk0

-- | Finishes up some book keeping about the Ui state.
tickUIFinish :: (UIState s m, MonadIO m) => m ()
tickUIFinish = do
  -- Update the cursor
  msaved <- use (ui . savedCursor)
  cursor <- use (ui . systemCursor)
  let mkNewCursor new = do
        newcursor <- io $ do newcursor <-createSystemCursor new
                             setCursor newcursor
                             return newcursor
        ui.savedCursor .= Just (new, newcursor)
  case msaved of
    Nothing -> if cursor == SDL_SYSTEM_CURSOR_ARROW
      then return ()
      else mkNewCursor cursor
    Just (oldname, old) -> if cursor == oldname
      then return ()
      else do
        io $ freeCursor old
        mkNewCursor cursor

tickPhysics :: (Physics s m, Time s m, MonadIO m) => m ()
tickPhysics = do
  oscene <- use scene
  -- Time is in milliseconds
  dt <- use (time.timeDelta)
  t0 <- use (time.timeLeft)
  let tt = dt + t0
      -- one physics step should be 0.01
      n = floor (fromIntegral tt / 10 :: Double)
      t1 = tt - (fromIntegral n * 10)
  time.timeLeft .= t1
  scene.scWorld .= runWorldOver 0.01 oscene n
