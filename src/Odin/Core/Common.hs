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
module Odin.Core.Common (
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
  , ReadsComponent
  , Modifies(..)
  , ModifiesComponent
  , DoesIO
  , MakesEntities
  -- * Sending / Receiving Messages
  , Mailbox
  , MakesMailbox(..)
  , SendsMessage(..)
  , GetsMessage(..)
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
import           Control.Concurrent.STM
--import           Control.Monad.Freer.State as Eff
--import           Control.Monad.Freer.Reader as Eff
--import           Control.Monad.Freer.Fresh as Eff
import           Control.Monad.State.Strict as State hiding (get,put,modify)
import qualified Control.Monad.State.Strict as S

import Odin.Core.Physics as OP
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

class (Monad m, Reads a m) => Modifies a m where
  get :: m a
  get = ask
  put    :: a -> m ()
  modify :: (a -> a) -> m ()

instance Reads (IntMap Name) System where
  ask = gets sysNames

instance Modifies (IntMap Name) System where
  put x = S.get >>= \s -> S.put s{sysNames = x}
  modify f = S.modify $ \s -> s{sysNames = f $ sysNames s}

instance Reads (IntMap PictureTransform) System where
  ask = gets sysTfrms

instance Modifies (IntMap PictureTransform) System where
  put x = S.get >>= \s -> S.put s{sysTfrms = x}
  modify f = S.modify $ \s -> s{sysTfrms = f $ sysTfrms s}

instance Reads (IntMap RenderIO) System where
  ask = gets sysRndrs

instance Modifies (IntMap RenderIO) System where
  put x = S.get >>= \s -> S.put s{sysRndrs = x}
  modify f = S.modify $ \s -> s{sysRndrs = f $ sysRndrs s}

instance Reads (IntMap DeallocIO) System where
  ask = gets sysDealloc

instance Modifies (IntMap DeallocIO) System where
  put x = S.get >>= \s -> S.put s{sysDealloc = x}
  modify f = S.modify $ \s -> s{sysDealloc = f $ sysDealloc s}

instance Reads (IntMap [Script]) System where
  ask = gets sysScripts

instance Modifies (IntMap [Script]) System where
  put x = S.get >>= \s -> S.put s{sysScripts = x}
  modify f = S.modify $ \s -> s{sysScripts = f $ sysScripts s}

instance Reads Int System where
  ask = gets sysFresh

instance Modifies Int System where
  put x = S.get >>= \s -> S.put s{sysFresh = x}
  modify f = S.modify $ \s -> s{sysFresh = f $ sysFresh s}

instance Reads Window System where
  ask = gets sysWindow

instance Reads Rez System where
  ask = gets sysRez

instance Reads [EventPayload] System where
  ask = gets sysEvents

instance Modifies [EventPayload] System where
  put x = S.get >>= \s -> S.put s{sysEvents= x}
  modify f = S.modify $ \s -> s{sysEvents= f $ sysEvents s}

instance Reads (Time) System where
  ask = gets sysTime

instance Modifies (Time) System where
  put x = S.get >>= \s -> S.put s{sysTime  = x}
  modify f = S.modify $ \s -> s{sysTime  = f $ sysTime  s}

instance Reads (OdinScene) System where
  ask = gets sysScene

instance Modifies (OdinScene) System where
  put x = S.get >>= \s -> S.put s{sysScene = x}
  modify f = S.modify $ \s -> s{sysScene = f $ sysScene s}

instance Reads (SystemOptions) System where
  ask = gets sysOptions

instance Modifies (SystemOptions) System where
  put x = S.get >>= \s -> S.put s{sysOptions = x}
  modify f = S.modify $ \s -> s{sysOptions = f $ sysOptions s}

instance Reads (SystemCommands) System where
  ask = gets sysCommands

instance Modifies (SystemCommands) System where
  put x = S.get >>= \s -> S.put s{sysCommands = x}
  modify f = S.modify $ \s -> s{sysCommands = f $ sysCommands s}

type ReadsComponent a = Reads (IntMap a)
type ModifiesComponent a = Modifies (IntMap a)
type DoesIO = MonadIO
type MakesEntities = Modifies Int
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
-- Sending / Recieving messages
--------------------------------------------------------------------------------
type Mailbox a = TVar (a -> System ())

class MakesMailbox a m where
  mailbox :: m (Mailbox a)

class SendsMessage a m where
  send :: Mailbox a -> a -> m ()

class GetsMessage a m where
  recv :: Mailbox a -> (a -> m ()) -> m ()

instance MakesMailbox a System where
  mailbox = io $ newTVarIO $ const $ return ()

instance SendsMessage a System where
  send mbox msg = do
    f <- io $ readTVarIO mbox
    f msg

instance GetsMessage a System where
  recv mbox f = void $ io $ atomically $ swapTVar mbox f
