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
        runBtn = do (eui, txt) <- capture $ buttonGUI "BUTTON"
                    liftIO $ print $ length <$> eui
                    return (eui, LoginCookie "")
        runLogin = (mempty,) <$> (transformGUI t $ loginAttempt Nothing)
        runInput = const (mempty, LoginCookie "") <$> testTextInput input
        input = initialize emptyTextInput $ do
                    textInputBox_.boxColor_ .= V4 0.3 0.3 0.3 1
                    textInputBox_.boxSize_ .= V2 200 32
                    textInputTransform_ .= Transform (V2 100 100) 1 0
    (eui, ck) <- eitherGUI runBtn $ eitherGUI runLogin runInput
    case eui of
        [] -> return ()
        ui -> do liftIO $ putStrLn "got some ui!"
                 gui (pure ui) never $ \_ _ -> ()

    liftIO $ print ck
    return ()
