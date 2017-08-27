{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
module Control.Monad.Trans.NextT
  ( module Control.Monad.Trans.NextT
  , NonEmpty(..)
  , MonadMask(..)
  ) where

import           Control.Monad.Catch       (MonadCatch (..), MonadMask (..),
                                            MonadThrow (..))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.List.NonEmpty        as NE

--------------------------------------------------------------------------------
-- Class
--------------------------------------------------------------------------------
class Monad m => MonadNext m where
  --type Base (m :: * -> *) :: * -> *
  -- | Done this loop immediately, returning a value.
  done :: a -> m a
  -- | Yield a computation to return to later (next frame).
  next :: m a -> m a
  -- | Run a computation to get either a value (in the case the loop done) or a
  -- computation to run later.
  runNext :: m a -> m (Either a (m a))

-- | A concrete implementation of MonadNext.
newtype NextT m b = NextT { runNextT :: m (Either b (NextT m b)) }

instance Monad m => MonadNext (NextT m) where
  done = NextT . return . Left
  next = NextT . return . Right
  runNext = lift . runNextT

--instance OdinReader
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Monad m => Functor (NextT m) where
  fmap f (NextT g) = NextT $ g >>= \case
    Right ev -> return $ Right $ fmap f ev
    Left c   -> return $ Left $ f c

instance Monad m => Applicative (NextT m) where
  pure = done
  ef <*> ex = do
    f <- ef
    x <- ex
    return $ f x

instance Monad m => Monad (NextT m) where
  (NextT g) >>= fev = NextT $ g >>= \case
    Right ev -> return $ Right $ ev >>= fev
    Left c -> runNextT $ fev c
  return = done

instance MonadTrans NextT where
  lift f = NextT $ f >>= return . Left

instance MonadIO m => MonadIO (NextT m) where
  liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (NextT m) where
  throwM = lift . throwM

instance MonadCatch m => MonadCatch (NextT m) where
  catch f h = catch f h

instance MonadMask m => MonadMask (NextT m) where
  mask = mask
  uninterruptibleMask = uninterruptibleMask
--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------
-- | Wait some number of frames.
wait :: MonadNext m => Int -> m ()
wait 0 = done ()
wait n = next $ wait $ n - 1

-- | Runs both evented computations (left and then right) each frame and returns
-- the first computation that completes.
withEither :: MonadNext m => m a -> m b -> m (Either a b)
withEither ea eb = ((,) <$> runNext ea <*> runNext eb) >>= \case
  ( Left a,       _) -> done $ Left a
  (      _,  Left b) -> done $ Right b
  (Right a, Right b) -> next $ withEither a b

-- | Runs all evented computations (left to right) on each frame and returns
-- the first computation that completes.
withAny :: MonadNext m => NonEmpty (m a) -> m a
withAny ts = go [] ts
  where go xs (y :| []) = runNext y >>= \case
          Left a -> done a
          Right f -> next $ withAny $ NE.fromList $ xs ++ [f]
        go xs (y :| (z:zs)) = runNext y >>= \case
          Left a -> done a
          Right f -> go (xs ++ [f]) (z :| zs)
