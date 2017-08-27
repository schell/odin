{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Odin.Engine.GUI.Animation where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Varying
import           Data.Functor.Identity  (Identity (..))
--------------------------------------------------------------------------------
import           Odin.Engine
import           Odin.Engine.Slots

newtype Anime b = Anime { unAnime :: Var Float b }

slotAnime :: (MonadIO m, MonadSafe m) => Var Float b -> m (Slot (Anime b))
slotAnime = slotVar . Anime

stepVarying :: Mutate SystemTime m => Var Float b -> m (b, Var Float b)
stepVarying v = runIdentity . runVarT v <$> readTimeDeltaSeconds

stepAnime :: Mutate SystemTime m => Slot (Anime b) -> m b
stepAnime s = do
  v0     <- fromSlot s unAnime
  (b, v) <- stepVarying v0
  s `is` Anime v
  return b
