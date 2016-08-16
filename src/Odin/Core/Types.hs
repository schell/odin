{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Odin.Core.Types where

import           Gelatin.SDL2 hiding (E)
import           SDL hiding (Event, get)
import           Data.IntMap.Strict (IntMap)
import           Data.Set (Set)
import           Data.Word (Word32)
import           Control.Monad.State.Strict

import Odin.Core.Physics as OP

newtype Uid = Uid { unUid :: Int } deriving (Show, Eq, Num, Enum)

data SystemTime = SystemTime { _timeLast  :: Word32
                             -- ^ The number of milliseconds from initialization to the last
                             -- time the system was ticked.
                             , _timeDelta :: Word32
                             -- ^ The difference between timeLast and the timeLast before it
                             , _timeLeft  :: Word32
                             -- ^ The number of milliseconds left unconsumed by the last
                             -- physics tick
                             } deriving (Show, Eq)

type Name = String

type Entity = Int

type RenderIO = PictureTransform -> IO ()

type DeallocIO = IO ()

type System = StateT Sys IO

type ScriptStep = System Script

data Script = Script { unScript :: System Script }
            | ScriptEnd

data SystemOption = SystemSkipPhysicsTick
                  | SystemDrawPhysicsTick
                  deriving (Show, Ord, Eq, Enum, Bounded)
type SystemOptions = Set SystemOption

data SystemCommand = SystemDeleteEntity Entity
                   deriving (Show, Ord, Eq, {-Enum, -}Bounded)
type SystemCommands = [SystemCommand]

data Sys = Sys { _sysNames    :: IntMap Name
               , _sysTfrms    :: IntMap PictureTransform
               , _sysRndrs    :: IntMap RenderIO
               , _sysDeallocs :: IntMap DeallocIO
               , _sysScripts  :: IntMap [Script]
               , _sysNextK    :: Int
               , _sysWindow   :: Window
               , _sysRez      :: Rez
               , _sysEvents   :: [EventPayload]
               , _sysTime     :: SystemTime
               , _sysScene    :: OdinScene
               , _sysOptions  :: SystemOptions
               , _sysCommands :: SystemCommands
               }
