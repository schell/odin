{-# LANGUAGE LambdaCase #-}
module Control.Monad.Evented where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Maybe

newtype EventT m b = EventT { runEventT :: m (Either (EventT m b) b) }
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

-- | Runs both evented computations (left and then right) each frame and returns
-- the first computation that completes.
withEither :: Monad m => EventT m a -> EventT m b -> EventT m (Either a b)
withEither ea eb = do
  lift ((,) <$> runEventT ea <*> runEventT eb) >>= \case
    (Right a,_) -> done $ Left a
    (_,Right b) -> done $ Right b
    (Left a, Left b) -> next $ withEither a b

-- | Runs all evented computations (left to right) on each frame and returns
-- the first computation that completes.
withAny :: Monad m => [EventT m a] -> EventT m a
withAny ts0 = do
  es <- lift $ mapM runEventT ts0
  case foldl f (Left []) es of
    Left ts -> next $ withAny ts
    Right a -> done a
  where f (Right a) _         = Right a
        f (Left ts) (Left t)  = Left $ ts ++ [t]
        f _         (Right a) = Right a
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

