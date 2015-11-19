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
--import Odin.Control.Common as C
import Odin.Data
import Odin.GUI
import Odin.Graphics.Types
import Linear hiding (trace, el)
import Gelatin.Core.Rendering hiding (polyline)
import Gelatin.Core.Color
import Control.Varying
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.RWS.Strict
import Control.Lens
import Data.Maybe
import Data.Renderable
import Data.Time.Clock

linePic :: Picture ()
linePic = do
    withFill (FillColor fill) line
    withStroke [StrokeColor white, StrokeFeather 1, StrokeWidth 4] line
    where line = polyline [V2 0 0, V2 100 100, V2 200 30, V2 400 50]
          fill (V2 x y) = V4 0 (x/400) (y/100) 1

rectPic = withFill (FillColor f) $ rectangle $ V2 100 100
    where f (V2 x y) = V4 (x/100) (y/100) 0 1

network :: SplineOf InputEvent (Picture ()) ()
network = do
    lns <- spline blank $ takeE 16 $ always $ do rectPic
                                                 linePic
    let n = withFill (FillTexture "/Users/schell/Desktop/leo-wtf-small.png" f) lns
        f (V2 x y) = V2 (x/400) (y/100)
    void $ pure n `untilEvent` (time ~> after 3)
    --_ <- textForm (pure mempty) ["Name: ", "Password: "] "Login"
    network

time :: MonadIO m => Var m a Float
time = delta (liftIO getCurrentTime) (\a b -> realToFrac $ diffUTCTime a b)

--field :: Maybe TextField -> Odin ()
--field mtxt = textField mempty mtxt >>= field . Just
--
--fields :: [String] -> [Maybe String -> Odin String]
--fields ss = zipWith f ss [(0 :: Int) ..]
--    where f s i mt = g <$> textField vt (Just $ mutate defaultTextField $ do
--                            textFieldLabel._2.plainTextString .= s
--                            textFieldInput.textInputText._2.plainTextString .= fromMaybe "" mt)
--                                where vt = pure t
--                                      t = Transform (V2 0 (32* fromIntegral i)) 1 0
--          txt = textFieldInput.textInputText._2.plainTextString
--          g = (^. txt)
