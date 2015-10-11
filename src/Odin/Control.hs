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
import Control.Concurrent.MVar
import Control.Lens
import Data.Monoid

network :: Odin ()
network = do
    -- Center the login card
    let s = ((+offset) . (*0.5)) <$> windowSize
        offset = - V2 200 50 :: V2 Float
        t = Transform <$> s <*> pure 1 <*> pure 0
        runBtn = buttonGUI "BUTTON" >> return ()
        runLogin = transformGUI t $ loginAttempt Nothing
        --runInput = const (LoginCookie "") <$> fs
    --ck <- eitherGUI runBtn $ eitherGUI runLogin runInput
    --liftIO $ print ck
    ss <- runGUIs (fields 3) (map show [0..]) runBtn
    liftIO $ print ss
    return ()

runGUIs :: (MonadIO m, Monoid (f b))
        => [c -> SplineT m f a b c] -> [c] -> SplineT m f a b () -> SplineT m f a b [Maybe c]
runGUIs fs xs = runGUIs' fs (replicate (length fs) NoEvent) $ zipWith ($) fs xs

runGUIs' :: (MonadIO m, Monoid (f b))
         => [c -> SplineT m f a b c] -> [Event c] -> [SplineT m f a b c] -> SplineT m f a b ()
         -> SplineT m f a b [Maybe c]
runGUIs' fs evs guis egui = SplineT $ Var $ \a -> do
    let step (ecs, fb, vs) (f, ec, gui) = do
            (Step fb' ec', v) <- runVar (runSplineT gui) a
            let ec'' = ec <> ec'
                fb'' = fb <> fb'
                v'   = case ec' of
                           NoEvent -> v
                           Event c -> runSplineT $ f c
            return (ecs ++ [ec''], fb'', vs ++ [SplineT v'])
    (ecs, fb, guis') <- foldM step ([],mempty,[]) (zip3 fs evs guis)
    (Step fb' ec, v) <- runVar (runSplineT egui) a
    let fb'' = fb <> fb'
        ec' = map toMaybe ecs <$ ec
    return (Step fb'' ec', runSplineT $ runGUIs' fs ecs guis' $ SplineT v)

fields :: Int -> [String -> Odin String]
fields n = map f [0.. fromIntegral n]
    where f n s = textInput $ initialize emptyTextInput $ do
                      textInputBox_.boxColor_ .= V4 0.3 0.3 0.3 1
                      textInputBox_.boxSize_ .= V2 200 20
                      textInputTransform_ .= Transform (V2 100 (100 + n * 22)) 1 0
                      textInputText_.plainTxtString_ .= s
