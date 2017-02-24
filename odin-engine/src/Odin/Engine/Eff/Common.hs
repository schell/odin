{-# LANGUAGE FlexibleContexts #-}
module Odin.Engine.Eff.Common
  ( module Odin.Engine.Eff.Common
    -- * Freer
  , module F
  ) where

import           Control.Monad.Freer as F
import           Control.Monad.Freer.State as F
import           Control.Monad.Freer.Reader as F

io :: Member IO r => IO a -> Eff r a
io = send

newtype Allocated = Allocated { unAllocated :: [IO ()] }
