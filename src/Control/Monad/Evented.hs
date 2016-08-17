{-# LANGUAGE LambdaCase #-}
module Control.Monad.Evented where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

--newtype EventT m a b c = EventT { unEventT :: a -> m (b, Maybe c) }
--
--runEventT :: Monad m => a -> EventT m a b c -> m (Either b c)
--runEventT a (EventT f) = f a >>= return . \case
--  (b, Nothing) -> Left b
--  (_, Just c)  -> Right c
--
--untilEvent :: (Functor m, Applicative m)
--           => (a -> m b) -> (a -> m (Maybe c)) -> EventT m a b c
--untilEvent amb ammc = EventT $ \a -> (,) <$> amb a <*> ammc a
--
--instance Monad m => Functor (EventT m a b) where
--  fmap f (EventT g) = EventT $ \a -> do
--    (b, mc) <- g a
--    return (b, f <$> mc)
--
----instance Monad (EventT m a b) where
----  (EventT ma) >>= f = EventT $ \a -> do
----    ma a >>= \case
----      (_, Just c) -> do
----        EventT g <- f c
----        g a
--
--
----instance Monoid b => Applicative (EventT m a b) where
----  pure x = EventT $ return (mempty, Just x)
----  (EventT f) <*> (EventT x) =

newtype EventT a b m c = EventT { runEventT :: a -> m (Either (EventT a b m c) c) }

instance Monad m => Functor (EventT a b m) where
  fmap f (EventT g) = EventT $ \a -> do
    g a >>= \case
      Left ev -> return $ Left $ fmap f ev
      Right c -> return $ Right $ f c

instance Monad m => Applicative (EventT a b m) where
  pure = return
  ef <*> ex = do
    f <- ef
    x <- ex
    return $ f x

instance Monad m => Monad (EventT a b m) where
  (EventT g) >>= fev = EventT $ \a -> do
    g a >>= \case
      Left ev -> return $ Left $ ev >>= fev
      Right c -> do let EventT h = fev c
                    h a
  return = EventT . const . return . Right

instance MonadTrans (EventT a b) where
  lift ma = EventT $ const $ ma >>= return . Right

instance MonadIO m => MonadIO (EventT a b m) where
  liftIO = lift . liftIO

waitUntil :: Monad m => m Bool -> EventT a b m ()
waitUntil f = EventT $ const $ do
  done <- f
  return $ if done then Right () else (Left $ waitUntil f)

waitUntilEither :: Monad m => m Bool -> m Bool -> EventT a b m (Either () ())
waitUntilEither fl fr = EventT $ const $ do
  isL <- fl
  isR <- fr
  let ev = if isL then Right $ Left ()
             else if isR then Right $ Right ()
                    else Left $ waitUntilEither fl fr
  return ev

