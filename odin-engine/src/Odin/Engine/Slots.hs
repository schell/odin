{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
module Odin.Engine.Slots
  ( MonadSafe (..)
  , autoRelease
  , Slot
  , slot
  , slotVar
  , unslot
  , releaseSlot
  , reslot
  , is
  , fromSlot
  , modifySlot
  , fromSlotM
  , modifySlotM
  ) where

import           Control.Concurrent.STM
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans    (lift)
import           Pipes.Safe             (MonadMask, MonadSafe (..), ReleaseKey,
                                         runSafeT)
--------------------------------------------------------------------------------
-- Auto releasing resources
--------------------------------------------------------------------------------
-- | Run a computation that registers finalizers for alloc'd resources, running
-- all unreleased finalizers before returning.
autoRelease :: (MonadIO m, MonadMask m) => m a -> m a
autoRelease = runSafeT . lift
--------------------------------------------------------------------------------
-- Storing / Retreiving mutable data
--------------------------------------------------------------------------------
data Slot a = Slot { unSlotVar :: TVar a
                   , unSlotKey :: Maybe ReleaseKey
                   }

-- | Safely slot a mutable resource and register a a finalizer for the resource.
slot :: (MonadIO m, MonadSafe m) => a -> (a -> IO ()) -> m (Slot a)
slot a free = do
  var <- liftIO $ newTVarIO a
  key <- register $ liftIO $ readTVarIO var >>= free
  return $ Slot var $ Just key

slotNoFree :: MonadIO m => a -> m (Slot a)
slotNoFree a = do
  var <- liftIO $ newTVarIO a
  return $ Slot var Nothing

slotVar :: MonadIO m => a -> m (Slot a)
slotVar = slotNoFree

unslot :: MonadIO m => Slot a -> m a
unslot = liftIO . readTVarIO . unSlotVar

releaseSlot :: (MonadIO m, MonadSafe m) => Slot a -> m ()
releaseSlot = maybe (return ()) release . unSlotKey

reslot :: MonadIO m => Slot a -> a -> m ()
reslot = ((void . liftIO . atomically) .) . swapTVar . unSlotVar

is :: MonadIO m => Slot a -> a -> m ()
is = reslot

fromSlot :: MonadIO m => Slot a -> (a -> b) -> m b
fromSlot s f = (f <$>) $ liftIO $ readTVarIO $ unSlotVar s

fromSlotM :: MonadIO m => Slot a -> (a -> m b) -> m b
fromSlotM s f = unslot s >>= f

modifySlot :: MonadIO m => Slot a -> (a -> a) -> m ()
modifySlot s = liftIO . atomically . modifyTVar' (unSlotVar s)

modifySlotM :: MonadIO m => Slot a -> (a -> m a) -> m ()
modifySlotM s f = unslot s >>= f >>= reslot s
