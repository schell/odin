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
  , runC'
  , Next
  , next
  , raceEither
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

-- | Reply to a coroutine effect by returning the Continue constructor.
replyC
  :: Yield a b c
  -> Arr r c (Status r a b w)
  -> Eff r (Status r a b w)
replyC (Yield a k) arr = return $ Continue a (arr . k)

-- | Launch a coroutine and report its status.
runC :: Eff (Yield a b ': effs) w -> Eff effs (Status effs a b w)
runC = handleRelay (return . Done) replyC

-- | Launch a coroutine and report its status, without handling (removing)
-- `Yield` from the typelist.
runC' :: Member (Yield a b) r => Eff r w -> Eff r (Status r a b w)
runC' = interpose (return . Done) replyC

type Next = Yield () ()

-- | Pause a coroutine effect to be picked up later. This is useful for control
-- flow.
next :: Member Next r => Eff r a -> Eff r a
next eff = do
  yield () $ \() -> ()
  eff

withEither'
  :: Member Next r
  => Status r () () a
  -> Status r () () b
  -> Eff r (Either a b)
withEither' (Done a) _                        = return $ Left a
withEither' _        (Done b)                 = return $ Right b
withEither' (Continue () fa) (Continue () fb) = next $
  fa () >>= \case
    Done a -> return $ Left a
    ca     -> fb () >>= \case
      Done b -> return $ Right b
      cb   -> withEither' ca cb

-- | Race two coroutine effects and return the result of the effect that finishes
-- first.
raceEither :: Member Next r => Eff r a -> Eff r b -> Eff r (Either a b)
raceEither effa effb = ((,) <$> runC' effa <*> runC' effb) >>= uncurry withEither'
