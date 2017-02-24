{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Odin.Engine.Checkpoint where

import           Data.Hashable
import           Data.Word
import           Foreign.Store
import           System.Environment

import           Odin.Engine.Eff.Common

-- | Takes a unique hashable key representing your value,
-- along with an action to create the first instance
-- of your value to be used on subsequent recompilations/revisitations.
checkpoint :: forall a r k. (Member IO r, Hashable k) => k -> Eff r a -> Eff r a
checkpoint k create = do
  let storeID = fromIntegral $ hash k
  -- See if an existing store exists.
  maybeStore :: Maybe (Store a) <- io (lookupStore storeID)
  case maybeStore of
    -- If so, return the value inside
    Just store -> io $ readStore store
    -- Otherwise, create the value, store it, and return it.
    Nothing -> do
      value <- create
      io $ writeStore (Store storeID) value
      return value
