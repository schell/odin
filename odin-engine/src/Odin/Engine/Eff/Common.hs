{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Odin.Engine.Eff.Common
  ( module Odin.Engine.Eff.Common
    -- * Freer
  , module F
  ) where

import           Control.Monad.Freer        as F
import           Control.Monad.Freer.Reader as F
import           Control.Monad.Freer.State  as F

io :: Member IO r => IO a -> Eff r a
io = send

data Allocated = Allocated { allocatedHere  :: [IO ()]
                           , allocatedThere :: Allocated
                           }
               | AllocatedNone

-- | Pushes a new allocation context onto the allocation stack.
pushHereToThere :: Member (State Allocated) r => Eff r ()
pushHereToThere = do
  here :: Allocated <- get
  put $ Allocated [] here

-- | Runs all the dealloc operations in the current allocation and returns the
-- remaining stack.
deallocHere :: Member IO r => Allocated -> Eff r Allocated
deallocHere = \case AllocatedNone        -> return AllocatedNone
                    Allocated here there -> do mapM_ io here
                                               return there

-- | Pops an allocation context off the allocation stack and runs all the
-- dealloc operations in the popped context.
popThereToHere :: (Member IO r, Member (State Allocated) r) => Eff r ()
popThereToHere = get >>= deallocHere >>= put

countAllocated :: Allocated -> Int
countAllocated AllocatedNone          = 0
countAllocated (Allocated here there) = length here + countAllocated there

deallocAllAllocated :: Member IO r => Allocated -> Eff r ()
deallocAllAllocated = \case AllocatedNone -> return ()
                            stack -> deallocHere stack >>= deallocAllAllocated
