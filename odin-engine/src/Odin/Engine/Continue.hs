{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Odin.Engine.Continue where

import           Control.Monad.Freer.Internal (interpose)
import           Odin.Engine.Eff

--newtype Cont r a = Cont {unCont :: Eff r a}
--
--
--runContinue :: Eff (Cont g ': r) a -> Eff r (Either a (Cont g))
--runContinue = handleRelay (return . Left) handler
--  where handler :: Cont r a
--                -> Arr r v a
--                -> Eff r a
--        handler = undefined

--replyC :: Cont r v
--        -> Arr r v (Either a (Cont r a))
--        -> Eff r (Either a (Cont r a))
--replyC (Cont f1) arr = return $ Right $ Cont $ f1 >>= \case
--  Left v  -> arr v
--  Right c -> return $ Right $ Cont $ replyC c arr
--
--handleC
--  :: Cont g v
--  -> Arr r v (Either a (Cont g a))
--  -> Eff r (Either a (Cont g a))
--handleC (Cont f1) arr = return $ Right $ Cont $ do
--  ev <- f1
--  case ev of
--    Left v  -> arr v
--    Right c -> return $ Right $ Cont $ handleC c arr
--
--runContinue
--  :: Eff (Cont g ': r) a -> Eff r a
--runContinue = handleRelay (return . Left) handleC
--
--runContinue' :: Member (Cont r) r => Eff r a -> Eff r (Either a (Cont r a))
--runContinue' = interpose (return . Left) replyC
--
--sendC :: Member (Cont r) r => Cont r a -> Eff r a
--sendC = send
--
--cont :: Member (Cont r) r => Eff r a -> Eff r a
--cont = sendC . Cont . runContinue'
--
--wait :: Member (Cont r) r => Int -> Eff r ()
--wait 0 = return ()
--wait n = cont $ wait $ n - 1
--
--withEither :: Member (Cont r) r => Eff r a -> Eff r b -> Eff r (Either a b)
--withEither fa fb = do
--  eac <- runContinue' fa
--  ebc <- runContinue' fb
--  withEither' eac ebc
--  where withEither'
--          :: Member (Cont r) r
--          => Either a (Cont r a)
--          -> Either b (Cont r b)
--          -> Eff r (Either a b)
--        withEither' (Left a) _        = return $ Left a
--        withEither' _        (Left b) = return $ Right b
--        withEither' ra       rb       = cont $ withEither' ra rb
