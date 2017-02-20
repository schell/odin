{-# LANGUAGE LambdaCase #-}
module Control.Monad.Evented where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

newtype EventT m b = EventT { runEventT :: m (Either b (EventT m b)) }
--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------
done :: Monad m => a -> EventT m a
done = EventT . return . Left

next :: Monad m => EventT m a -> EventT m a
next = EventT . return . Right
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
withEither ea eb =
  lift ((,) <$> runEventT ea <*> runEventT eb) >>= \case
    (Left a,_) -> done $ Left a
    (_,Left b) -> done $ Right b
    (Right a, Right b) -> next $ withEither a b

-- | Runs all evented computations (left to right) on each frame and returns
-- the first computation that completes.
withAny :: Monad m => [EventT m a] -> EventT m a
withAny ts0 = do
  es <- lift $ mapM runEventT ts0
  case foldl f (Right []) es of
    Right ts -> next $ withAny ts
    Left a   -> done a
  where f (Left a) _           = Left a
        f (Right ts) (Right t) = Right $ ts ++ [t]
        f _         (Left a)   = Left a
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Monad m => Functor (EventT m) where
  fmap f (EventT g) = EventT $
    g >>= \case
      Right ev -> return $ Right $ fmap f ev
      Left c -> return $ Left $ f c

instance Monad m => Applicative (EventT m) where
  pure = done
  ef <*> ex = do
    f <- ef
    x <- ex
    return $ f x

instance Monad m => Monad (EventT m) where
  (EventT g) >>= fev = EventT $ g >>= \case
    Right ev -> return $ Right $ ev >>= fev
    Left c -> runEventT $ fev c
  return = done

instance MonadTrans EventT where
  lift f = EventT $ f >>= return . Left

instance MonadIO m => MonadIO (EventT m) where
  liftIO = lift . liftIO
