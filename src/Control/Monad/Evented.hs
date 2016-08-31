{-# LANGUAGE LambdaCase #-}
module Control.Monad.Evented where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Maybe

newtype EventT m b = EventT { runEventT :: m (Either (EventT m b) b) }
--------------------------------------------------------------------------------
-- Runners
--------------------------------------------------------------------------------
runUntilDone :: Monad m => EventT m b -> m b
runUntilDone f = runEventT f >>= \case
  Left g  -> runUntilDone g
  Right b -> return b
--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------
done :: Monad m => a -> EventT m a
done = EventT . return . Right

next :: Monad m => EventT m a -> EventT m a
next = EventT . return . Left
--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------
-- | Waits a number of frames.
wait :: Monad m => Int -> EventT m ()
wait 0 = done ()
wait n = next $ wait $ n - 1

waitUntil :: Monad m => m Bool -> EventT m ()
waitUntil f = do
  isdone <- lift f
  if isdone then return () else (next $ waitUntil f)

waitUntilEither :: Monad m => m Bool -> m Bool -> EventT m (Either () ())
waitUntilEither fl fr = do
  isL <- lift fl
  isR <- lift fr
  if isL then done (Left ())
         else if isR then done (Right ())
                     else next $ waitUntilEither fl fr

waitUntilJust :: Monad m => m (Maybe b) -> EventT m b
waitUntilJust f = lift f >>= \case
  Just c  -> done c
  Nothing -> next $ waitUntilJust f

waitUntilAny :: Monad m => [m (Maybe b)] -> EventT m b
waitUntilAny fs = do
  mcs <- lift $ sequence fs
  case catMaybes mcs of
    c:_ -> done c
    []  -> next $ waitUntilAny fs
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Monad m => Functor (EventT m) where
  fmap f (EventT g) = EventT $ do
    g >>= \case
      Left ev -> return $ Left $ fmap f ev
      Right c -> return $ Right $ f c

instance Monad m => Applicative (EventT m) where
  pure = done
  ef <*> ex = do
    f <- ef
    x <- ex
    return $ f x

instance Monad m => Monad (EventT m) where
  (EventT g) >>= fev = EventT $ g >>= \case
    Left ev -> return $ Left $ ev >>= fev
    Right c -> runEventT $ fev c
  return = done

instance MonadTrans EventT where
  lift f = EventT $ f >>= return . Right

instance MonadIO m => MonadIO (EventT m) where
  liftIO = lift . liftIO

