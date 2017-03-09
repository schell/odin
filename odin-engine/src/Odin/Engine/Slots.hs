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
  ( Allocates
  , Allocated(..)
  , autoRelease
  , Slot
  , slot
  , slotVar
  , unslot
  , reslot
  , ($=)
  , is
  , fromSlot
  , modifySlot
  , fromSlotM
  , modifySlotM
  ) where

import           Control.Concurrent.STM
import           Control.Monad             (void)
import           Control.Monad.Freer.State
--------------------------------------------------------------------------------
import           Odin.Engine.Eff.Common
--------------------------------------------------------------------------------
-- Auto releasing IO resources
--------------------------------------------------------------------------------
type Allocates = State Allocated

alloc :: Member Allocates r => IO () -> Eff r ()
alloc f = modify (Allocated . (f:) . unAllocated)

autoRelease :: (Member Allocates r, Member IO r) => Eff r a -> Eff r a
autoRelease eff = do
  Allocated previousAllocs <- get
  put $ Allocated []
  a <- eff
  Allocated newAllocs <- get
  io $ putStrLn $ "Dealloc'ing: " ++ show (length newAllocs)
  mapM_ io newAllocs
  io $ putStrLn $ "Restoring: " ++ show (length previousAllocs)
  put $ Allocated previousAllocs
  return a

getNumAllocated :: Member Allocates r => Eff r Int
getNumAllocated = do
  Allocated allocs <- get
  return $ length allocs
--------------------------------------------------------------------------------
-- Storing / Retreiving mutable data
--------------------------------------------------------------------------------
newtype Slot a = Slot { unSlot :: TVar a }

slot :: (Member Allocates r, Member IO r) => a -> (a -> IO ()) -> Eff r (Slot a)
slot a free = do
  var <- io $ newTVarIO a
  alloc $ readTVarIO var >>= free
  getNumAllocated >>= io . putStrLn . ("Have alloc'd " ++) . show
  return $ Slot var

slotNoFree :: Member IO r => a -> Eff r (Slot a)
slotNoFree = (Slot <$>) . io . newTVarIO

slotVar :: Member IO r => a -> Eff r (Slot a)
slotVar = slotNoFree

unslot :: Member IO r => Slot a -> Eff r a
unslot = io . readTVarIO . unSlot

reslot :: Member IO r => Slot a -> a -> Eff r ()
reslot = ((void . io . atomically) .) . swapTVar . unSlot

($=) :: Member IO r => Slot a -> a -> Eff r ()
($=) = reslot

is :: Member IO r => Slot a -> a -> Eff r ()
is = reslot

fromSlot :: Member IO r => Slot a -> (a -> b) -> Eff r b
fromSlot s f = (f <$>) $ io $ readTVarIO $ unSlot s

fromSlotM :: Member IO r => Slot a -> (a -> Eff r b) -> Eff r b
fromSlotM s f = unslot s >>= f

modifySlot :: Member IO r => Slot a -> (a -> a) -> Eff r ()
modifySlot s = io . atomically . modifyTVar' (unSlot s)

modifySlotM :: Member IO r => Slot a -> (a -> Eff r a) -> Eff r ()
modifySlotM s f = unslot s >>= f >>= reslot s