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
module Odin.Core.Common
  -- * Reexporting Effects
  ( module State
  -- * Reexporting Linear
  , module L
  -- * Reexporting Lens pieces
  , use
  -- * Rexporting Common Vector Typeclasses
  , Unbox
  -- * Entities
  , Entity
  , Uid(..)
  -- * Components
  , Name
  , RenderIO
  , RenderGUI
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
  , fonts
  -- * Working with fonts/atlases
  , loadAtlas
  , saveAtlas
  , fontDescriptor
  , getFontPath
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
  , Fonts
  , Reads(..)
  -- * Storing / Retreiving Values
  , Slot
  , allocSlot
  , readSlot
  , fromSlot
  , swapSlot
  , modifySlot
  -- * Fonts
  , FontDescriptor(..)
  -- * Painting / Graphics
  , Painting(..)
  , Painter(..)
  , paintingBounds
  , paintingSize
  , paintingOrigin
  , paintingCenter
  , RenderTransform(..)
  , renderToPictureTransform
  , rendersToPictureTransform
  , move
  , scale
  , rotate
  , multiply
  , redChannelReplacement
  , moveV2
  , scaleV2
  , multiplyV4
  , redChannelReplacementV4
  -- * Time savers / Helpers
  , io
  -- * Experiments
  , Frame(..)
  , UpdateT
  ) where

import           Gelatin.SDL2 hiding (E, move, scale, rotate, multiply)
import           Gelatin.FreeType2
import           SDL hiding (Event, get, time)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.IntMap.Strict (IntMap)
import           Data.Vector.Unboxed (Unbox)
import           Control.Concurrent.STM
import           Control.Monad.State.Strict as State
import           Control.Lens
import           Linear as L hiding (rotate)
import           System.FilePath
import           System.Directory

import           Control.Monad.Evented as E

import           Odin.Core.Physics as OP
import           Odin.Core.Types
--------------------------------------------------------------------------------
-- Experiments
--------------------------------------------------------------------------------
newtype FontDescriptor = FontDescriptor (FilePath, GlyphSize)
                       deriving (Show, Eq, Ord)

type FontMap = Map FontDescriptor Atlas

data Frame = Frame { _frameTime   :: SystemTime
                   , _frameEvents :: [EventPayload]
                   , _frameNextK  :: Int
                   , _frameWindow :: Window
                   , _frameRez    :: Rez
                   , _frameScene  :: OdinScene
                   , _frameFonts  :: FontMap
                   }
makeLenses ''Frame
makeFields ''Frame

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
type Fonts s m      = (MonadState s m, HasFonts    s FontMap)
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

renderPainting :: MonadIO m => [RenderTransform] -> Painting -> m ()
renderPainting rs p = do
  let t = rendersToPictureTransform rs
      r = snd $ snd $ unPainting p
  liftIO $ r t

freePainting :: MonadIO m => Painting -> m ()
freePainting = liftIO . fst . snd . unPainting


