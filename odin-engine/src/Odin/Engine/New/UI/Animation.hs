{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Odin.Engine.New.UI.Animation where

import           Control.Monad.Fix          (MonadFix)
import           Control.Varying            hiding (Event)
import           Data.Functor.Identity      (Identity (..))
import           Reflex.SDL2


-- | Integrates a pure varying network graph into reflex. Returns a dynamic of
-- the graph output.
varying
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Var a b
  -- ^ The varying network graph.
  -> a
  -- ^ The initial input.
  -> Event t a
  -- ^ An event that provides successive input.
  -> m (Dynamic t b)
varying v a0 = (fmap fst <$>) . foldDyn (\t (_, vn) -> runVarying vn t) (runVarying v a0)
  where runVarying v1 = runIdentity . runVarT v1
