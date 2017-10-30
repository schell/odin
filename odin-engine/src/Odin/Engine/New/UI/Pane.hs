{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Odin.Engine.New.UI.Pane
  ( pane
  , PaneCfg (..)
  ) where

import           Control.Arrow              ((&&&))
import           Control.Monad              (forM_, msum, unless, void)
import           Control.Monad.Reader       (local)
import           Control.Monad.Trans        (lift)
import qualified Data.Map                   as M
import           Data.Word                  (Word64)
import           Foreign.Marshal            hiding (void)
import           Gelatin.SDL2               hiding (move, rotate, scale)
import           Reflex.SDL2                hiding (fan)
import           Reflex.SDL2.Internal

import           Odin.Engine.New
import           Odin.Engine.New.UI.Configs
import           Odin.Engine.New.UI.Layer


--------------------------------------------------------------------------------
data PaneInternal = PaneInternal { piK         :: Word64
                                 , piWidgets   :: [Widget]
                                 }


pane
  :: forall r t m a. OdinWidget r t m
  => Shape
  -- ^ The initial shape of the layer's boundary.
  -> V4 Float
  -- ^ The initial background color.
  -> V2 Float
  -- ^ The initial scroll offset (as an X and Y percentage value between 0 and 1)
  -> PaneCfg t
  -- ^ Any event based updates.
  -> DynamicWriterT t [Widget] m a
  -- ^ The widgets to run within the pane.
  -> m a
pane boundIni colorIni scrollIni paneCfg subwidgets = do
  let layerCfg = def & setBoundaryEvent .~ (paneCfg ^. setBoundaryEvent)

  (a, dWidgets) <- runDynamicWriterT $
    layer boundIni colorIni layerCfg $ lift subwidgets

  tellDyn dWidgets
  return a
