{-# LANGUAGE LambdaCase #-}
module Control.Monad.Evented where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Maybe

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
--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------
done :: Monad m => c -> EventT a b m c
done = return

next :: Monad m => EventT a b m c -> EventT a b m c
next = EventT . const . return . Left

-- | Waits a number of frames.
wait :: Monad m => Int -> EventT a b m ()
wait 0 = done ()
wait n = next $ wait $ n - 1

waitUntil :: Monad m => m Bool -> EventT a b m ()
waitUntil f = do
  isdone <- lift f
  if isdone then return () else (next $ waitUntil f)

waitUntilEither :: Monad m => m Bool -> m Bool -> EventT a b m (Either () ())
waitUntilEither fl fr = do
  isL <- lift fl
  isR <- lift fr
  if isL then done (Left ())
         else if isR then done (Right ())
                     else next $ waitUntilEither fl fr

waitUntilJust :: Monad m => m (Maybe c) -> EventT a b m c
waitUntilJust f = lift f >>= \case
  Just c  -> done c
  Nothing -> next $ waitUntilJust f

waitUntilAny :: Monad m => [m (Maybe c)] -> EventT a b m c
waitUntilAny fs = do
  mcs <- lift $ sequence fs
  case catMaybes mcs of
    c:_ -> done c
    []  -> next $ waitUntilAny fs
