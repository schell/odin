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
  --, ReservedEntities(..)
  -- * Components
  , Component
  , Time(..)
  , Name
  , RenderIO
  , DeallocIO
  -- * Component Systems
  , System
  , SystemOption(..)
  , SystemOptions
  , SystemCommand(..)
  , SystemCommands
  , SystemStep(..)
  , emptySystemStep
  , module OP
  -- * Scripts
  , ScriptStep
  , Script(..)
  , isRunningScript
  , runScript
  , nextScript
  , endScript
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
import           Data.Set (Set)
import           Data.Word (Word32)
import           Control.Monad.Freer as Eff
import           Control.Monad.Freer.State as Eff
import           Control.Monad.Freer.Reader as Eff
import           Control.Monad.Freer.Fresh as Eff
--import qualified Control.Monad.State.Strict as MTL

import Odin.Physics as OP
--------------------------------------------------------------------------------
-- Odin Component/System Types
--------------------------------------------------------------------------------
type Pic = Picture GLuint ()

data Time = Time { timeLast  :: Word32
                 , timeDelta :: Float
                 , timeLeft  :: Float
                 -- ^ The amount of time left unconsumed by the last physics tick
                 } deriving (Show, Eq)

type Name = String

type Entity = Int

type Component a = State (IntMap a)
--type ComponentMTL a = MTL.State (IntMap a)

type RenderIO = Rendering IO PictureTransform
type DeallocIO = CleanOp IO

data SystemOption = SystemSkipPhysicsTick
                  | SystemDrawPhysicsTick
                  deriving (Show, Ord, Eq, Enum, Bounded)
type SystemOptions = Set SystemOption

data SystemCommand = SystemDeleteEntity Entity
                   deriving (Show, Ord, Eq, {-Enum, -}Bounded)
type SystemCommands = [SystemCommand]

type System = Eff '[Component Name
                   ,Component PictureTransform
                   ,Component RenderIO
                   ,Component DeallocIO
                   ,Component [Script]
                   ,Fresh
                   ,Reader Window
                   ,Reader Rez
                   ,State [EventPayload]
                   ,State Time
                   ,State OdinScene
                   ,State SystemOptions
                   ,State SystemCommands
                   ,IO
                   ]

type ScriptStep = System Script

data Script = Script { unScript :: ScriptStep }
            | ScriptEnd

data SystemStep = SystemStep { sysNames    :: IntMap Name
                             , sysTfrms    :: IntMap PictureTransform
                             , sysRndrs    :: IntMap RenderIO
                             , sysDealloc  :: IntMap DeallocIO
                             , sysScripts  :: IntMap [Script]
                             , sysFresh    :: Int
                             , sysWindow   :: Window
                             , sysRez      :: Rez
                             , sysEvents   :: [EventPayload]
                             , sysTime     :: Time
                             , sysScene    :: OdinScene
                             , sysOptions  :: SystemOptions
                             , sysCommands :: SystemCommands
                             }

--data ReservedEntities = ReservedPhysicsDrawingEntity
--                      deriving (Show, Ord, Eq, Enum, Bounded)

emptySystemStep :: Rez -> Window -> SystemStep
emptySystemStep rez window =
  SystemStep mempty mempty mempty mempty mempty k window rez [] (Time 0 0 0) emptyScene mempty mempty
    where k = 0 --length [ReservedPhysicsDrawingEntity ..]
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

endScript :: Monad m => m Script
endScript = return ScriptEnd

nextScript :: Monad m => ScriptStep -> m Script
nextScript = return . Script
