{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
module Odin.Engine.Eff.Coroutine
  ( Yield
  , yield
  , Status (..)
  , runC
  , Next
  , next
  ) where

import           Control.Monad.Freer.Internal

-- | A type representing a yielding of control.
--
-- Type variables have following meaning:
--
-- [@a@]
--   The current type.
--
-- [@b@]
--   The input to the continuation function.
--
-- [@c@]
--   The output of the continuation.
data Yield a b c = Yield a (b -> c)
  deriving (Functor)

-- | Lifts a value and a function into the Coroutine effect.
yield :: Member (Yield a b) effs => a -> (b -> c) -> Eff effs c
yield x f = send (Yield x f)

-- | Represents status of a coroutine.
data Status effs a b x
    = Done x
    -- ^ Coroutine is done.
    | Continue a (b -> Eff effs (Status effs a b x))
    -- ^ Reporting a value of the type @a@, and resuming with the value of type
    -- @b@.

-- | Launch a coroutine and report its status.
runC :: Eff (Yield a b ': effs) w -> Eff effs (Status effs a b w)
runC = handleRelay (return . Done) handler
  where
    handler
        :: Yield a b c
        -> Arr effs c (Status effs a b w)
        -> Eff effs (Status effs a b w)
    handler (Yield a k) arr = return $ Continue a (arr . k)

type Next = Yield () ()

next :: Member (Yield () ()) r => Eff r a -> Eff r a
next eff = do
  yield () $ \() -> ()
  eff
