{-# LANGUAGE FlexibleContexts #-}
module Odin.Engine.Eff.Common
  ( module Odin.Engine.Eff.Common
    -- * Freer
  , module Control.Monad.Freer
  , runState
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.State

io :: Member IO r => IO a -> Eff r a
io = send
