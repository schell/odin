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
module Odin.Core.Common
  -- * Reexporting Effects
  ( module State
  -- * Reexporting Linear
  , module L
  -- * Rexporting Common Vector Typeclasses
  , Unbox
  -- * Entities
  , Entity
  , Uid(..)
  -- * Components
  , Name
  , RenderIO
  , DeallocIO
  , Script(..)
  , module OP
  -- * The System
  , System
  , SystemTime(..)
  , timeDelta
  , timeLast
  , timeLeft
  , SystemOption(..)
  , SystemOptions
  , SystemCommand(..)
  , SystemCommands
  , Sys(..)
  , sysCommands
  , commands
  , sysDeallocs
  , deallocs
  , sysEvents
  , events
  , sysNextK
  , nextK
  , sysNames
  , names
  , sysOptions
  , options
  , sysRez
  , rez
  , sysRndrs
  , rndrs
  , sysScene
  , scene
  , sysScripts
  , scripts
  , sysTfrms
  , tfrms
  , sysTime
  , time
  , sysWindow
  , window
  , emptySys
  -- * Scripts
  , ScriptStep
  , isRunningScript
  , runScript
  , nextScript
  , endScript
  -- * Sequenced Events
  , module E
  , Evented
  , runEventedScript
  -- * Constraints / Abilities
  , Names
  , Tfrms
  , Rndrs
  , Deallocs
  , Scripts
  , Fresh
  , Events
  , Time
  , Physics
  , Options
  , Commands
  , DoesIO
  , Rezed
  , Windowed
  , Reads(..)
  -- * Storing / Retreiving Values
  , Slot
  , allocSlot
  , readSlot
  , fromSlot
  , swapSlot
  , modifySlot
  -- * Painting / Graphics
  , Painting(..)
  , Painter(..)
  , runPainterT
  , runPainterBounds
  , runPainterSize
  , runPainterOrigin
  , runPainterCenter
  , compilePainter
  , compilePainterZ
  , compilePaintings
  , RenderTransform(..)
  , renderToPictureTransform
  , rendersToPictureTransform
  -- * Time savers / Helpers
  , io
  -- * Experiments
  , Frame(..)
  , UpdateT
  ) where

import           Gelatin.SDL2 hiding (E)
import           SDL hiding (Event, get, time)
import           Data.IntMap.Strict (IntMap)
import           Data.Vector.Unboxed (Unbox)
import           Control.Concurrent.STM
import           Control.Monad.State.Strict as State
import           Control.Lens
import           Linear as L

import           Control.Monad.Evented as E

import           Odin.Core.Physics as OP
import           Odin.Core.Types
--------------------------------------------------------------------------------
-- Experiments
--------------------------------------------------------------------------------
data Frame = Frame { _frameTime   :: SystemTime
                   , _frameEvents :: [EventPayload]
                   , _frameNextK  :: Int
                   , _frameWindow :: Window
                   , _frameRez    :: Rez
                   , _frameScene  :: OdinScene
                   }
makeLenses ''Frame
makeFields ''Frame

data RenderTransform = Spatial (Affine (V2 Float) Float)
                     | Alpha Float
                     | Multiply (V4 Float)
                     | ColorReplacement (V4 Float)

renderToPictureTransform :: RenderTransform -> PictureTransform
renderToPictureTransform (Spatial affine) =
  PictureTransform (affineToModelview affine) 1 1 Nothing
renderToPictureTransform (Alpha a) = PictureTransform identity a 1 Nothing
renderToPictureTransform (Multiply c) = PictureTransform identity 1 c Nothing
renderToPictureTransform (ColorReplacement c) = PictureTransform identity 1 1 (Just c)

rendersToPictureTransform :: [RenderTransform] -> PictureTransform
rendersToPictureTransform = mconcat . map renderToPictureTransform

type UpdateT m = EventT (StateT Frame m)

instance Monad m => MonadState Frame (UpdateT m) where
  get = lift get
  put = lift . put
--------------------------------------------------------------------------------
-- Odin Component/System Constraints and Abilities
--------------------------------------------------------------------------------
makeLenses ''SystemTime
makeLenses ''Sys
makeFields ''Sys

type Names s m      = (MonadState s m, HasNames    s (IntMap String))
type Tfrms s m      = (MonadState s m, HasTfrms    s (IntMap PictureTransform))
type Rndrs s m      = (MonadState s m, HasRndrs    s (IntMap RenderIO))
type Deallocs s m   = (MonadState s m, HasDeallocs s (IntMap DeallocIO))
type Scripts s m    = (MonadState s m, HasScripts  s (IntMap [Script]))
type Fresh s m      = (MonadState s m, HasNextK    s Int)
type Events s m     = (MonadState s m, HasEvents   s [EventPayload])
type Time s m       = (MonadState s m, HasTime     s SystemTime)
type Physics s m    = (MonadState s m, HasScene    s OdinScene)
type Options s m    = (MonadState s m, HasOptions  s SystemOptions)
type Commands s m   = (MonadState s m, HasCommands s SystemCommands)
type Windowed s m   = (MonadState s m, HasWindow   s Window)
type Rezed s m      = (MonadState s m, HasRez      s Rez)
type DoesIO = MonadIO

class Monad m => Reads a m where
  ask :: m a

instance Reads Window System where
  ask = use window

instance Reads Rez System where
  ask = use rez

instance Reads SystemTime System where
  ask = use time

emptySys :: Rez -> Window -> Sys
emptySys rz win =
  Sys mempty mempty mempty mempty mempty 0 win rz [] (SystemTime 0 0 0) emptyScene mempty mempty
--------------------------------------------------------------------------------
-- Doing IO
--------------------------------------------------------------------------------
io :: MonadIO m => IO a -> m a
io = liftIO
--------------------------------------------------------------------------------
-- Scripts
--------------------------------------------------------------------------------
isRunningScript :: Script -> Bool
isRunningScript ScriptEnd = False
isRunningScript _ = True

runScript :: Script -> System Script
runScript (Script s) = s
runScript ScriptEnd = return ScriptEnd

endScript :: Monad m => m Script
endScript = return ScriptEnd

nextScript :: Monad m => ScriptStep -> m Script
nextScript = return . Script
--------------------------------------------------------------------------------
-- Sequenced Events
--------------------------------------------------------------------------------
type Evented a = EventT System a

runEventedScript :: Evented a -> System Script
runEventedScript ev = runEventT ev >>= \case
  Left nv -> return $ Script $ runEventedScript nv
  Right _ -> endScript
--------------------------------------------------------------------------------
-- Storing / Retreiving messages
--------------------------------------------------------------------------------
newtype Slot a = Slot { unSlot :: TVar a }

allocSlot :: MonadIO m => a -> m (Slot a)
allocSlot = (Slot <$>) . io . newTVarIO

readSlot :: MonadIO m => Slot a -> m a
readSlot = io . readTVarIO . unSlot

fromSlot :: MonadIO m => Slot a -> (a -> b) -> m b
fromSlot s f = (f <$>) $ io $ readTVarIO $ unSlot s

swapSlot :: MonadIO m => Slot a -> a -> m ()
swapSlot s a = void $ io $ atomically $ swapTVar (unSlot s) a

modifySlot :: MonadIO m => Slot a -> (a -> a) -> m ()
modifySlot s = io . atomically . modifyTVar' (unSlot s)
--------------------------------------------------------------------------------
-- Painting / Rendering
--------------------------------------------------------------------------------
compilePaintings :: MonadIO m => Rez -> GLRenderer -> Painting m -> m GLRenderer
compilePaintings rz r0 (ColorPainting pic) = do
  (_,dat) <- runPictureT pic
  r1 <- liftIO $ compileColorPictureData rz dat
  return $ r0 `mappend` r1
compilePaintings rz r0 (TexturePainting pic) = do
  (_,dat) <- runPictureT pic
  r1 <- liftIO $ compileTexturePictureData rz dat
  return $ r0 `mappend` r1

runPainterT :: MonadIO m  => Rez -> Painter a m -> a -> m GLRenderer
runPainterT rz f a = do
  paintings <- (unPainter f) a
  foldM (compilePaintings rz) mempty paintings

compilePainter :: (Rezed s m, DoesIO m) => Painter a m -> a -> m GLRenderer
compilePainter painter a = do
  rz <- use rez
  runPainterT rz painter a

compilePainterZ :: MonadIO m => Rez -> Painter a m -> a -> m GLRenderer
compilePainterZ rz painter a = runPainterT rz painter a

runPainterBounds :: MonadIO m => Painter a m -> a -> m (V2 Float, V2 Float)
runPainterBounds f a = do
  vss <- mapM measurePainting =<< (unPainter f) a
  return $ pointsBounds $ concat vss
  where measurePainting :: MonadIO m => Painting m -> m [V2 Float]
        measurePainting (ColorPainting pic) = do
          (tl,br) <- runPictureBoundsT pic
          return [tl,br]
        measurePainting (TexturePainting pic) = do
          (tl,br) <- runPictureBoundsT pic
          return [tl,br]

runPainterSize :: MonadIO m => Painter a m -> a -> m (V2 Float)
runPainterSize f a = do
  (tl,br) <- runPainterBounds f a
  return $ br - tl

runPainterOrigin :: MonadIO m => Painter a m -> a -> m (V2 Float)
runPainterOrigin f a = runPainterBounds f a >>= return . fst

runPainterCenter :: MonadIO m => Painter a m -> a -> m (V2 Float)
runPainterCenter f a = do
  (tl,br) <- runPainterBounds f a
  return $ tl + (br - tl)/2
