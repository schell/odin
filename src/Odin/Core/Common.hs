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
module Odin.Core.Common (
  -- * Reexporting Effects
    module State
  -- * Entities
  , Entity
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
  , Reads(..)
  -- * Sending / Receiving Messages
  , MailboxT
  , Mailbox
  , mailbox
  , send
  , recv
  -- * Time savers / Helpers
  , io
  ) where

import           Gelatin.SDL2 hiding (E)
import           SDL hiding (Event, get, time)
import           Data.IntMap.Strict (IntMap)
import           Control.Concurrent.STM
import           Control.Monad.State.Strict as State
import           Control.Lens

import           Odin.Core.Physics as OP
import           Odin.Core.Types
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
-- Sending / Recieving messages
--------------------------------------------------------------------------------
type MailboxT m a = TVar (a -> m ())
type Mailbox a = MailboxT System a

mailbox :: MonadIO m => m (MailboxT m a)
mailbox = io $ newTVarIO $ const $ return ()

send :: MonadIO m => MailboxT m a -> a -> m ()
send mbox msg = do
  f <- io $ readTVarIO mbox
  f msg

recv :: MonadIO m => MailboxT m a -> (a -> m ()) -> m ()
recv mbox f = void $ io $ atomically $ swapTVar mbox f
