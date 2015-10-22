{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Odin.Control (
    module C,
    network
) where

import Odin.Control.TextField
--import Odin.Control.TextForm
import Odin.Control.Button
import Odin.Control.Common as C
import Odin.Data
import Odin.GUI
import Linear hiding (trace, el)
import Gelatin.Core.Rendering hiding (polyline)
import Control.Varying
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.RWS.Strict
import Control.Lens
import Data.Maybe
import Data.Renderable

network :: SplineOf InputEvent (Picture ()) ()
network = do
    lns <- spline empty $ takeE 16 $ always $ polyline [V2 0 0, V2 100 100, V2 200 30, V2 400 50]
    let n = do rectangle $ V2 100 100
               lns
    void $ pure n `untilEvent` (time ~> after 3)
    --_ <- textForm (pure mempty) ["Name: ", "Password: "] "Login"
    network

field :: Maybe TextField -> Odin ()
field mtxt = textField mempty mtxt >>= field . Just

fields :: [String] -> [Maybe String -> Odin String]
fields ss = zipWith f ss [(0 :: Int) ..]
    where f s i mt = g <$> textField vt (Just $ mutate defaultTextField $ do
                            textFieldLabel._2.plainTextString .= s
                            textFieldInput.textInputText._2.plainTextString .= fromMaybe "" mt)
                                where vt = pure t
                                      t = Transform (V2 0 (32* fromIntegral i)) 1 0
          txt = textFieldInput.textInputText._2.plainTextString
          g = (^. txt)

