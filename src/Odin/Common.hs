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
module Odin.Common (
  -- * Reexporting Effects
    module State
  -- * Entities
  , Entity
  --, ReservedEntities(..)
  -- * Components
  --, Component
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
  , Reads(..)
  , Modifies(..)
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
import           Data.FTCQueue
--import           Control.Monad.Freer.State as Eff
--import           Control.Monad.Freer.Reader as Eff
--import           Control.Monad.Freer.Fresh as Eff
import           Control.Monad.State.Strict as State hiding (get,put,modify)
import qualified Control.Monad.State.Strict as S

import Odin.Physics as OP
--------------------------------------------------------------------------------
-- Odin Component/System Types
--------------------------------------------------------------------------------
type Pic = Picture GLuint ()

data Time = Time { timeLast  :: Word32
                 -- ^ The number of milliseconds from initialization to the last
                 -- time the system was ticked.
                 , timeDelta :: Word32
                 -- ^ The difference between timeLast and the timeLast before it
                 , timeLeft  :: Word32
                 -- ^ The number of milliseconds left unconsumed by the last
                 -- physics tick
                 } deriving (Show, Eq)

type Name = String

type Entity = Int

--type Component a = State (IntMap a)

type RenderIO = Rendering IO PictureTransform
type DeallocIO = CleanOp IO

data SystemOption = SystemSkipPhysicsTick
                  | SystemDrawPhysicsTick
                  deriving (Show, Ord, Eq, Enum, Bounded)
type SystemOptions = Set SystemOption

data SystemCommand = SystemDeleteEntity Entity
                   deriving (Show, Ord, Eq, {-Enum, -}Bounded)
type SystemCommands = [SystemCommand]

type Message a = (String, a)

type Mailbox = _

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

type System = StateT SystemStep IO

type ScriptStep = System Script

data Script = Script { unScript :: ScriptStep }
            | ScriptEnd

emptySystemStep :: Rez -> Window -> SystemStep
emptySystemStep rez window =
  SystemStep mempty mempty mempty mempty mempty k window rez [] (Time 0 0 0) emptyScene mempty mempty
    where k = 0 --length [ReservedPhysicsDrawingEntity ..]
--------------------------------------------------------------------------------
-- System Type Constraints (energy savers)
--------------------------------------------------------------------------------
class Monad m => Reads a m where
  ask :: m a

instance Reads Window System where
  ask = gets sysWindow

instance Reads Rez System where
  ask = gets sysRez

instance Reads Time System where
  ask = gets sysTime

class Monad m => Modifies a m where
  get    :: m a
  put    :: a -> m ()
  modify :: (a -> a) -> m ()

instance Modifies (IntMap Name) System where
  get = gets sysNames
  put x = S.get >>= \s -> S.put s{sysNames = x}
  modify f = S.modify $ \s -> s{sysNames = f $ sysNames s}

instance Modifies (IntMap PictureTransform) System where
  get = gets sysTfrms
  put x = S.get >>= \s -> S.put s{sysTfrms = x}
  modify f = S.modify $ \s -> s{sysTfrms = f $ sysTfrms s}

instance Modifies (IntMap RenderIO) System where
  get = gets sysRndrs
  put x = S.get >>= \s -> S.put s{sysRndrs = x}
  modify f = S.modify $ \s -> s{sysRndrs = f $ sysRndrs s}

instance Modifies (IntMap DeallocIO) System where
  get = gets sysDealloc
  put x = S.get >>= \s -> S.put s{sysDealloc = x}
  modify f = S.modify $ \s -> s{sysDealloc = f $ sysDealloc s}

instance Modifies (IntMap [Script]) System where
  get = gets sysScripts
  put x = S.get >>= \s -> S.put s{sysScripts = x}
  modify f = S.modify $ \s -> s{sysScripts = f $ sysScripts s}

instance Modifies (Int) System where
  get = gets sysFresh
  put x = S.get >>= \s -> S.put s{sysFresh = x}
  modify f = S.modify $ \s -> s{sysFresh = f $ sysFresh s}

instance Modifies [EventPayload] System where
  get = gets sysEvents
  put x = S.get >>= \s -> S.put s{sysEvents= x}
  modify f = S.modify $ \s -> s{sysEvents= f $ sysEvents s}

instance Modifies (Time) System where
  get = gets sysTime
  put x = S.get >>= \s -> S.put s{sysTime  = x}
  modify f = S.modify $ \s -> s{sysTime  = f $ sysTime  s}

instance Modifies (OdinScene) System where
  get = gets sysScene
  put x = S.get >>= \s -> S.put s{sysScene = x}
  modify f = S.modify $ \s -> s{sysScene = f $ sysScene s}

instance Modifies (SystemOptions) System where
  get = gets sysOptions
  put x = S.get >>= \s -> S.put s{sysOptions = x}
  modify f = S.modify $ \s -> s{sysOptions = f $ sysOptions s}

instance Modifies (SystemCommands) System where
  get = gets sysCommands
  put x = S.get >>= \s -> S.put s{sysCommands = x}
  modify f = S.modify $ \s -> s{sysCommands = f $ sysCommands s}

type ModifiesComponent a = Modifies (IntMap a)
type DoesIO = MonadIO
type MakesEntities = Modifies Int
--------------------------------------------------------------------------------
-- Doing IO
--------------------------------------------------------------------------------
--io :: Member IO r => IO a -> Eff r a
--io = send
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
