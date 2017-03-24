{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Odin.Engine.Checkpoint
  ( Checkpoint
  , readCheckpoint
  , updateCheckpoint
  , withCheckpoint
  , checkpoint
  ) where

import           Data.Hashable
import           Data.String            (IsString (..))
import           Data.Word              (Word32)
import           Foreign.Store

import           Odin.Engine.Eff.Common

newtype Checkpoint a = Checkpoint { unCheckpoint :: String } deriving (Show, Eq)

instance IsString (Checkpoint a) where
  fromString = Checkpoint

storeID :: Checkpoint a -> Word32
storeID = fromIntegral . hash . unCheckpoint

-- | Takes a unique key representing your checkpoint and might return a value
-- stored at that checkpoint.
readCheckpoint :: forall a r. Member IO r => Checkpoint a -> Eff r (Maybe a)
readCheckpoint k = do
  -- See if an existing store exists.
  maybeStore :: Maybe (Store a) <- io $ lookupStore $ storeID k
  io $ sequence $ readStore <$> maybeStore

-- | Takes a unique key representing your checkpoint along with a value and
-- stores the value at that checkpoint.
updateCheckpoint :: forall a r. Member IO r => Checkpoint a -> a -> Eff r ()
updateCheckpoint k v = io $ writeStore (Store $ storeID k) v

-- | Takes a unique key representing your checkpoint, along with an action to
-- create the first instance of your value if it doesn't exist, as well as an
-- effectful handler to use on the value.
withCheckpoint :: Member IO r => Checkpoint a -> Eff r a -> (a -> Eff r b) -> Eff r b
withCheckpoint k create handle = readCheckpoint k >>= \case
  Just v  -> handle v
  Nothing -> do
    v <- create
    io $ writeStore (Store $ storeID k) v
    handle v

-- | Takes a unique key representing your checkpoint, along with an action to
-- create the first instance of your value to be used on subsequent
-- recompilations/revisitations.
checkpoint :: forall a r. Member IO r => Checkpoint a -> Eff r a -> Eff r a
checkpoint k create = withCheckpoint k create return
