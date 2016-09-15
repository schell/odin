{-# LANGUAGE FlexibleContexts #-}
module Odin.GUI.Animation.Internal where

import Odin.Core
import Control.Varying hiding (use)
import Control.Lens (use)

newtype Anime m b = Anime { unAnime :: VarT m Float b }

allocAnime :: MonadIO m => VarT m Float b -> m (Slot (Anime m b))
allocAnime = allocSlot . Anime

stepAnime :: (MonadIO m, Time s m) => Slot (Anime m b) -> m b
stepAnime s = do
  v0     <- fromSlot s unAnime
  dt     <- readTimeDeltaSeconds
  (b, v) <- runVarT v0 dt
  swapSlot s $ Anime v
  return b
