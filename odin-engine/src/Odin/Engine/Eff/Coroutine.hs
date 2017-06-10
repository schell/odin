{-# LANGUAGE DataKinds             #-}
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
  , nextForever
  , raceEither
  , raceAny
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Data.Function                 (fix)

type Next = Yield () ()

-- | Pause a coroutine effect to be picked up later. This is useful for control
-- flow.
next :: Member Next r => Eff r a -> Eff r a
next eff = do
  yield () $ \() -> ()
  eff

-- | Run a computation and yield, then pick back up where you left off again,
-- forever.
nextForever :: Member Next r => Eff r a -> Eff r b
nextForever eff = fix $ \loop -> eff >> next loop

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

-- | Race all of the coroutine effects until one finishes (which means the
-- computation completes without yielding.)
raceAny :: Member Next r => [Eff r a] -> Eff r a
raceAny effs = mapM interposeC effs >>= checkStats
  where checkStats stats = case foldl f (Right []) stats of
          Right ts -> next $ sequence ts >>= checkStats
          Left a   -> return a
        f (Left a)   _               = Left a
        f (Right ts) (Continue () g) = Right $ ts ++ [g ()]
        f (Right _ ) (Done a)        = Left a
