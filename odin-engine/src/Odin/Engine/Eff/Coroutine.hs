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
  ( Next
  , next
  , raceEither
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine

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
raceEither effa effb = ((,) <$> interposeC effa <*> interposeC effb) >>= uncurry withEither'
