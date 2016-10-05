{-# LANGUAGE FlexibleContexts #-}
module Odin.GUI.Animation.Internal where

import Odin.Core
import Control.Varying hiding (use)

newtype Anime m b = Anime { unAnime :: VarT m Float b }

slotAnime :: MonadIO m => VarT m Float b -> m (Slot (Anime m b))
slotAnime = slot . Anime

stepAnime :: (MonadIO m, Time s m) => Slot (Anime m b) -> m b
stepAnime s = do
  v0     <- fromSlot s unAnime
  dt     <- readTimeDeltaSeconds
  (b, v) <- runVarT v0 dt
  s `is` Anime v
  return b
