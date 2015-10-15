{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Odin.Control (
    module C,
    network
) where

import Odin.Control.TextField
import Odin.Control.Button
import Odin.Control.Common as C
import Odin.Data
import Linear hiding (trace, el)
import Gelatin.Core.Rendering
import Control.Varying
import Control.Monad.IO.Class
import Control.Monad
import Control.Lens
import Data.Maybe

network :: Odin ()
network = do
    let btn = void $ button (pure $ Transform (V2 0 150) 1 0) "GO"
        fs = fields $ map show ([0..3] :: [Int])
    form <- mix fs btn
    liftIO $ print form
    return ()

fields :: [String] -> [Maybe String -> Odin String]
fields ss = zipWith f ss [0 ..]
    where f s i mt = g <$> textField vt (Just $ mutate defaultTextField $ do
                            textFieldLabel._2.plainTextString .= s
                            textFieldInput.textInputText._2.plainTextString .= fromMaybe "" mt)
                                where vt = pure $ Transform (V2 0 (32*i)) 1 0
          txt = textFieldInput.textInputText._2.plainTextString
          g = (^. txt)

