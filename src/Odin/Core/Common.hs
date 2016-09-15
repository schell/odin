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
import           SDL hiding (Event, get, time)
import           Data.Monoid ((<>))
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import           Data.Vector.Unboxed (Unbox)
import           Data.Word (Word32)
import           Control.Concurrent.STM
import           Control.Monad.State.Strict as State
import           Control.Lens
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

data Frame = Frame { _frameTime   :: SystemTime
                   , _frameEvents :: [EventPayload]
                   , _frameNextK  :: Int
                   , _frameWindow :: Window
                   , _frameRez    :: Rez
                   , _frameFonts  :: FontMap
                   , _frameRsrcs  :: [IO ()]
                   }
makeLenses ''Frame
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

type Fresh s m      = (MonadState s m, HasNextK    s Int)
type Events s m     = (MonadState s m, HasEvents   s [EventPayload])
type Time s m       = (MonadState s m, HasTime     s SystemTime)
type Physics s m    = (MonadState s m, HasScene    s OdinScene)
type Windowed s m   = (MonadState s m, HasWindow   s Window)
type Rezed s m      = (MonadState s m, HasRez      s Rez)
type Fonts s m      = (MonadState s m, HasFonts    s FontMap)
type Resources s m  = (MonadState s m, HasRsrcs    s [IO ()])
--------------------------------------------------------------------------------
-- Time Savers/Aliases
--------------------------------------------------------------------------------
io :: MonadIO m => IO a -> m a
io = liftIO
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

allocSlot :: MonadIO m => a -> m (Slot a)
allocSlot = (Slot <$>) . io . newTVarIO

slot :: MonadIO m => a -> m (Slot a)
slot = allocSlot

readSlot :: MonadIO m => Slot a -> m a
readSlot = io . readTVarIO . unSlot

unslot :: MonadIO m => Slot a -> m a
unslot = readSlot

fromSlot :: MonadIO m => Slot a -> (a -> b) -> m b
fromSlot s f = (f <$>) $ io $ readTVarIO $ unSlot s

fromSlotM :: MonadIO m => Slot a -> (a -> m b) -> m b
fromSlotM s f = readSlot s >>= f

swapSlot :: MonadIO m => Slot a -> a -> m ()
swapSlot s a = void $ io $ atomically $ swapTVar (unSlot s) a

is :: MonadIO m => Slot a -> a -> m ()
is = swapSlot

modifySlot :: MonadIO m => Slot a -> (a -> a) -> m ()
modifySlot s = io . atomically . modifyTVar' (unSlot s)

modifySlotM :: MonadIO m => Slot a -> (a -> m a) -> m ()
modifySlotM s f = readSlot s >>= f >>= swapSlot s
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

tickEvents :: (Events s m, MonadIO m) => m ()
tickEvents = do
  evs <- io (pollEvents >>= mapM (processEvent . eventPayload))
  events .= evs
  where processEvent QuitEvent = exitSuccess
        processEvent ev@(KeyboardEvent (KeyboardEventData _ _ _ k)) = do
          when (isQuit k) exitSuccess
          --print (m,r,k)
          return ev
        processEvent e = return e

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
