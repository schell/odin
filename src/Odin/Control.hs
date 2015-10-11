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
import Control.Varying.Time
import Control.GUI hiding (before, after)
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens
import Data.Monoid

network :: Odin ()
network = do
    -- Center the login card
    let --s = ((+offset) . (*0.5)) <$> windowSize
        --offset = - V2 200 50 :: V2 Float
        --t = Transform <$> s <*> pure 1 <*> pure 0
        runBtn = buttonGUI "BUTTON" >> return ()
        --runLogin = transformGUI t $ loginAttempt Nothing
        --runInput = const (LoginCookie "") <$> fs
    --ck <- eitherGUI runBtn $ eitherGUI runLogin runInput
    --liftIO $ print ck
    (ui, ss) <- capture $ runGUIs (fields 3) runBtn
    let g = () <$ gui (pure ui) (time ~> after 2)
    _ <- flip transformGUI g (time ~> tfrm)
    liftIO $ print ss
    return ()

tfrm :: MonadIO m => Var m Float Transform
tfrm = Transform <$> (V2 <$> scl <*> 0) <*> pure 1 <*> pure 0

scl :: MonadIO m => Var m Float Float
scl = execSpline 1 $ do
    x <- tweenTo easeOutExpo 0 20 0.5
    tweenTo easeInExpo x (-100) 1

fields :: Int -> [Maybe String -> Odin String]
fields n = map f [0.. fromIntegral n]
    where f n ms = textInput $ initialize emptyTextInput $ do
                      textInputBox_.boxColor_ .= V4 0.3 0.3 0.3 1
                      textInputBox_.boxSize_ .= V2 200 20
                      textInputTransform_ .= Transform (V2 100 (100 + n * 22)) 1 0
                      textInputText_.plainTxtString_ .= s
                          where s = case ms of
                                        Just s' -> s'
                                        Nothing -> ""

