{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Odin.Engine.Eff.Fresh
  ( Fresh
  , fresh
  , runFresh
  ) where

import           Odin.Engine.Eff.Common

-- | Fresh effect model
data Fresh v where
  Fresh :: Fresh Int

-- | Request a fresh effect
fresh :: Member Fresh r => Eff r Int
fresh = send Fresh

-- | Handler for Fresh effects, with an Int for a starting value
runFresh :: Eff (Fresh ': r) w -> Int -> Eff r (w, Int)
runFresh m s =
  handleRelayS s (\_s x -> return (x, _s))
                 (\s' Fresh k -> (k $! s'+1) s')
                 m
