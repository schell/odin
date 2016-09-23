{-# LANGUAGE FlexibleContexts #-}
module Odin.GUI.Common where

import Control.Lens
import Odin.Core

-- | Runs any IO ops stored in frameRsrcs.
freeResources :: (MonadIO m, Resources s m) => m ()
freeResources = do
  use rsrcs >>= mapM_ io
  rsrcs .= []

-- | Register an IO op to free some resources later.
registerFree :: Resources s m => IO () -> m ()
registerFree f = rsrcs %= (f:)

-- | Brackets a bunch of alloc'ing calls and automatically calls their
-- registered clean up operations.
autoReleaseResources :: (MonadIO m, Resources s m) => m a -> m a
autoReleaseResources f = do
  srcs <- use rsrcs
  rsrcs .= []
  x    <- f
  freeResources
  rsrcs .= srcs
  return x

-- | A version of `autoReleaseResources` that discards its result value.
autoReleaseResources_ :: (MonadIO m, Resources s m) => m a -> m ()
autoReleaseResources_ f = autoReleaseResources f >> return ()

-- | Ignores all `registerFree` calls made within the block.
ignoreResources :: (MonadIO m, Resources s m) => m a -> m a
ignoreResources f = do
  srcs <- use rsrcs
  x    <- f
  rsrcs .= srcs
  return x
