{-# LANGUAGE FlexibleContexts #-}
module Odin.Control where

import Odin.Graphics.Renderable
import Odin.Data

import Data.Renderable
import Data.Time.Clock
import Linear
import Gelatin.Core.Color
import Gelatin.Core.Rendering
import Control.Varying
import Control.Varying.Time
import Control.Eff
import Control.Eff.Lift

network :: SetMember Lift (Lift IO) r => Var (Eff r) InputEvent UI
network =
    Element <$>
        sequenceA [ pure $ Element $ PlainText tfrm "module Blah where" white
                  , pure $ Element $ Icon itfrm "\xf0c7" white
                  , Element <$> box
                  ]
    where
          tfrm = Transform (V2 100 100) (V2 1 1) 0
          itfrm = Transform (V2 200 200) (V2 1 1) 0
          box :: SetMember Lift (Lift IO) r => Var (Eff r) InputEvent Box
          box = Box mempty (V2 100 50) <$> (time ~> (purple `alpha`) <$> blink)
          blink :: SetMember Lift (Lift IO) r => Var (Eff r) Float Float
          blink = once 0 `andThenE` tween easeInExpo 0 1 0.3 `andThenE`
                       tween easeInExpo 1 0 0.8 `andThen` blink

time :: (Fractional t, SetMember Lift (Lift IO) r) => Var (Eff r) a t
time = delta (lift $ getCurrentTime) (\a b -> realToFrac $ diffUTCTime a b)
