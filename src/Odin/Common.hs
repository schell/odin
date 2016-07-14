{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Odin.Common (
  -- * Reexporting Effects
    module Eff
  -- * Entities
  , Entity
  -- * Components
  , Component
  , Time(..)
  , Name(..)
  , RenderIO
  , DeallocIO
  -- * Component Systems
  , System
  , SystemStep(..)
  , emptySystemStep
  -- * Scripts
  , ScriptStep
  , Script(..)
  , isRunningScript
  , runScript
  -- * Constraints / Abilities
  , Reads
  , Modifies
  , ModifiesComponent
  , DoesIO
  , MakesEntities
  -- * Time savers / Helpers
  , io
  , Pic
  ) where

import           Gelatin.Picture
import           Gelatin.SDL2 hiding (E)
import           SDL hiding (Event, get)
import           Data.IntMap.Strict (IntMap)
import           Data.Word (Word32)
import           Control.Monad.Freer as Eff
import           Control.Monad.Freer.State as Eff
import           Control.Monad.Freer.Reader as Eff
import           Control.Monad.Freer.Fresh as Eff
--------------------------------------------------------------------------------
-- Odin Component/System Types
--------------------------------------------------------------------------------
type Pic = Picture GLuint ()

data Time = Time { timeLast  :: Word32
                 , timeDelta :: Float
                 } deriving (Show, Eq)

newtype Name = Name String

type Entity = Int

type Component a = State (IntMap a)

type RenderIO = Rendering IO PictureTransform
type DeallocIO = CleanOp IO

type System = Eff '[Component Name
                   ,Component PictureTransform
                   ,Component RenderIO
                   ,Component DeallocIO
                   ,Fresh
                   ,Reader Window
                   ,Reader Rez
                   ,State [EventPayload]
                   ,State Time
                   ,State [Script]
                   ,IO
                   ]

type ScriptStep = System Script

data Script = Script { unScript :: ScriptStep }
            | ScriptEnd

data SystemStep = SystemStep { sysNames  :: IntMap Name
                             , sysTfrms  :: IntMap PictureTransform
                             , sysRndrs  :: IntMap RenderIO
                             , sysDealloc:: IntMap DeallocIO
                             , sysFresh  :: Int
                             , sysWindow :: Window
                             , sysRez    :: Rez
                             , sysEvents :: [EventPayload]
                             , sysTime   :: Time
                             , sysScripts:: [Script]
                             }

emptySystemStep :: Rez -> Window -> SystemStep
emptySystemStep rez window =
  SystemStep mempty mempty mempty mempty 0 window rez [] (Time 0 0) []
--------------------------------------------------------------------------------
-- System Type Constraints (energy savers)
--------------------------------------------------------------------------------
type Reads a = Member (Reader a)
type Modifies a = Member (State a)
type ModifiesComponent a = Modifies (IntMap a)
type DoesIO = Member IO
type MakesEntities = Member Fresh
--------------------------------------------------------------------------------
-- Doing IO
--------------------------------------------------------------------------------
io :: Member IO r => IO a -> Eff r a
io = send
--------------------------------------------------------------------------------
-- Scripts
--------------------------------------------------------------------------------
isRunningScript :: Script -> Bool
isRunningScript ScriptEnd = False
isRunningScript _ = True

runScript :: Script -> System Script
runScript (Script s) = s
runScript ScriptEnd = return ScriptEnd
