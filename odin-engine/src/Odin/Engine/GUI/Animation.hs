{-# LANGUAGE FlexibleContexts #-}
module Odin.Engine.GUI.Animation where

import Control.Varying
import Data.Functor.Identity (Identity(..))
--------------------------------------------------------------------------------
import Odin.Engine.Slots
import Odin.Engine.Eff

newtype Anime b = Anime { unAnime :: Var Float b }

slotAnime :: Member IO r => Var Float b -> Eff r (Slot (Anime b))
slotAnime = slotVar . Anime

stepAnime :: (Member IO r, AltersTime r) => Slot (Anime b) -> Eff r b
stepAnime s = do
  v0     <- fromSlot s unAnime
  dt     <- readTimeDeltaSeconds
  let Identity (b, v) = runVarT v0 dt
  s `is` Anime v
  return b
