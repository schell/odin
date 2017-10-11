{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Odin.Engine.New.EventT where

import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class (MonadIO (..))
import           Reflex.SDL2


newtype EventT t m a = EventT { runEventT :: m (Event t a) }


instance (Reflex t, Functor m) => Functor (EventT t m) where
  fmap f (EventT ma) = EventT $ fmap f <$> ma


instance (Reflex t, PostBuild t m, MonadFix m, MonadHold t m, MonadAdjust t m)
  => Applicative (EventT t m) where
  pure = return
  mf <*> mx = do
    f <- mf
    x <- mx
    return $ f x


instance (Reflex t, Monad m, PostBuild t m, MonadFix m, MonadHold t m, MonadAdjust t m)
  => Monad (EventT t m) where
  return x = EventT $ do
    evPB <- getPostBuild
    return $ x <$ evPB
  m1 >>= fm2 = EventT $ mdo
    (ev1, evEv2) <- runWithReplace (runEventT m1) ev1Next
    let ev1Next = runEventT . fm2 <$> ev1
    switchPromptly never evEv2


hoistE
  :: (PostBuild t m, MonadFix m, MonadHold t m, MonadAdjust t m, MonadIO m)
  => m a
  -> EventT t m a
hoistE ma = EventT $ do
  a  <- ma
  ev <- getPostBuild
  return $ a <$ ev


instance (PostBuild t m, MonadFix m, MonadHold t m, MonadAdjust t m, MonadIO m) => MonadIO (EventT t m) where
  liftIO = hoistE . liftIO


liftE :: (Reflex t, MonadHold t m, MonadFix m) => m (Event t a) -> EventT t m a
liftE = EventT . (headE =<<)


untilE
  :: (Monad m, Reflex t, MonadHold t m, MonadFix m, MonadAdjust t m)
  => m a -> Event t b -> EventT t m (a, b)
untilE ma ev = EventT $ do
  ev0 <- fmap return <$> headE ev
  (a, ev1) <- runWithReplace ma ev0
  return $ (a,) <$> ev1


untilE_
  :: (Monad m, Reflex t, MonadHold t m, MonadFix m, MonadAdjust t m)
  => m a -> Event t b -> EventT t m a
untilE_ ma ev = fst <$> untilE ma ev


_untilE
  :: (Monad m, Reflex t, MonadHold t m, MonadFix m, MonadAdjust t m)
  => m a -> Event t b -> EventT t m b
_untilE ma ev = snd <$> untilE ma ev
