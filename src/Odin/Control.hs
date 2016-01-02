{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Odin.Control (
    --module C,
    network
) where

--import Odin.Control.TextField
--import Odin.Control.TextForm
--import Odin.Control.Button
import Odin.Control.Common as C
import Odin.Data
import Odin.GUI
import Odin.Data.Common
import Odin.Control.File
import Linear hiding (trace, el)
import Gelatin.Core.Rendering hiding (polyline)
import Gelatin.Core.Color
import Graphics.Text.TrueType
import Control.Varying
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.RWS.Strict
import Control.Lens
import Data.Maybe
import Data.Renderable
import Data.Time.Clock
import System.Exit

linePic,rectPic,hello :: Picture ()
linePic = do
    withFill (FillColor fill) line
    withStroke [StrokeColor white, StrokeFeather 1, StrokeWidth 4] $ do
        line
        withTransform (Transform (V2 20 100) 1 0) $
            letters 16 "Hello!"
    where line = polyline [V2 0 0, V2 100 100, V2 200 30, V2 400 50]
          fill (V2 x y) = V4 0 (x/400) (y/100) 1

rectPic =
    withTransform (Transform 100 1 0) $
        withFill (FillColor f) $ rectangle $ V2 400 400
    where f (V2 x y) = V4 (x/400) (y/400) 0 1

hello =
    withTransform (Transform 100 1 0) $ do
        withFill (solid white) $
            letters 64 "Hello."
        withStroke [StrokeColor red, StrokeWidth 2, StrokeFeather 1] $
            letters 64 "Hello."

network :: SplineOf InputEvent (Picture ()) ()
network = do
    let ariald = FontDescriptor "Arial" $ FontStyle False False
        hackd = FontDescriptor "Hack" $ FontStyle False False
    (_,arial) <- pure rectPic `untilEvent` getFont ariald
    (_,hack) <- pure rectPic `untilEvent` getFont hackd
    spline blank $ always $ do
        let (w,h)
        withFont arial hello --graph ~> onWhen (const True)
        withFont arial linePic
    network

time :: MonadIO m => Var m a Float
time = delta (liftIO getCurrentTime) (\a b -> realToFrac $ diffUTCTime a b)

