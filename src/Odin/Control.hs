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
import Linear hiding (trace, el, point)
import Gelatin.Core.Rendering hiding (polyline, scale)
import Gelatin.Core.Color
import Gelatin.Core.Rendering.Bezier
import Gelatin.Core.Rendering.Shape
import Graphics.Text.TrueType
import Control.Varying
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.RWS.Strict
import Control.Lens hiding (transform)
import Data.Maybe
import Data.Renderable
import Data.Time.Clock
import System.Exit

rect :: Picture ()
rect = withFill (solid grey) $ rectangle 100

errorScreen sz = withTransform (Transform (sz/2) 1 0) $
    withFill (solid red) $ rectangle sz

boundedPic textPic = do
    let fill = withFill (solid white) textPic
        stroke = withStroke [StrokeWidth 1, StrokeFeather 0, StrokeColor white] $
                    textPic
        bounds = withTransform (Transform (V2 (w/2) (-h/2)) 1 0) $
                    withStroke [StrokeWidth 1, StrokeFeather 0, StrokeColor red] $
                        rectangle sz
        sz@(V2 w h) = pictureSize textPic
    bounds
    fill
    withTransform (Transform (V2 (w+2) 0) 1 0) $ do
        bounds
        stroke

next :: (Applicative m, Monad m, Monoid b) => Var m a (Event b) -> Spline a b m b
next = spline mempty

next_ :: (Applicative m, Monad m, Monoid b) => Var m a (Event b) -> Spline a b m ()
next_ v = do _ <- next v
             return ()

network :: App ()
network = do
    (arial,hack) <- liftIO $ do
        marialp <- findFontOfFamily "Arial" $ FontStyle False False
        mhackp <- findFontOfFamily "Hack" $ FontStyle False False
        mearial <- sequence (loadFontFile <$> marialp)
        mehack  <- sequence (loadFontFile <$> mhackp)

        let f mefont = case mefont of
                           Just (Left str) -> do putStrLn str
                                                 exitFailure
                           Just (Right fnt) -> return fnt
                           Nothing -> exitFailure
        arial <- f mearial
        hack  <- f mehack
        return (arial,hack)

    next_ $ once $ move 200 rect
    sz <- lift $ asks _readWindowSize


    let arialText = withFont arial $ letters 16 "This is Arial."
        hackText = withFont hack $ letters 16 "This is Hack."
        text = do move 100 $ boundedPic arialText
                  move (V2 100 120) $ boundedPic hackText
                  let r = 10
                      p = 2 + (V2 r r)
                  move p $ withFill (solid pink) $ circle 10
                  move (sz/2) $ withStroke [ StrokeWidth 4
                                           , StrokeFeather 1
                                           , StrokeColors [white,yellow,orange,red]
                                           ] $
                    do (circle 100)
                       let line = bez4sToPath 100 0 $ arc 100 50 (pi - pi/8) $ (2*pi - pi/8)
                       move 50 $ polyline line

    _ <- pure text `untilEvent` enter

    liftIO exitSuccess
