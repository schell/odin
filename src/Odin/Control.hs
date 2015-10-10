{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Odin.Control (
    module C,
    network
) where

import Odin.Control.Login
import Odin.Control.TextInput
import Odin.Control.Button
import Odin.Control.Common as C
import Odin.Data
import Caltrops.Client
import Linear hiding (trace, el)
import Gelatin.Core.Rendering
import Control.Varying
import Control.GUI
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens

network :: Odin ()
network = do
    -- Center the login card
    let s = ((+offset) . (*0.5)) <$> windowSize
        offset = - V2 200 50 :: V2 Float
        t = Transform <$> s <*> pure 1 <*> pure 0
        runBtn = do _ <- buttonGUI "BUTTON"
                    return (LoginCookie "")
        runLogin = transformGUI t $ loginAttempt Nothing
        runInput = const (LoginCookie "") <$> testTextInput input
        input = initialize emptyTextInput $ do
                    textInputBox_.boxColor_ .= V4 0.3 0.3 0.3 1
                    textInputBox_.boxSize_ .= V2 200 32
                    textInputTransform_ .= Transform (V2 100 100) 1 0
    ck <- eitherGUI runBtn $ eitherGUI runLogin runInput
    liftIO $ print ck
    return ()
