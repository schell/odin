{-# LANGUAGE FlexibleContexts #-}
module Odin.Engine.GUI.Animation where

import Control.Varying
--------------------------------------------------------------------------------
import Odin.Engine.Slots
import Odin.Engine.Eff

newtype Anime r b = Anime { unAnime :: VarT (Eff r) Float b }

slotAnime :: Member IO r => VarT (Eff r) Float b -> Eff r (Slot (Anime r b))
slotAnime = slotNoFree . Anime

stepAnime :: (Member IO r, AltersTime r) => Slot (Anime r b) -> Eff r b
stepAnime s = do
  v0     <- fromSlot s unAnime
  dt     <- readTimeDeltaSeconds
  (b, v) <- runVarT v0 dt
  s $= Anime v
  return b
