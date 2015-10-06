{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Odin.Control (
    module C,
    network
) where

import Odin.Control.Login
import Odin.Control.TextInput
import Odin.Control.Common as C
import Odin.Data
import Linear hiding (trace, el)
import Gelatin.Core.Rendering
import Control.Varying
import Control.GUI
import Control.Monad.IO.Class

network :: GUI IO InputEvent UI ()
network = do
    -- Center the login card
    let s = ((+offset) . (*0.5)) <$> (windowSizeStream ~> startingWith 0)
        offset = - V2 200 50 :: V2 Float
        t = Transform <$> s <*> pure 1 <*> pure 0
        login = transformGUI t $ loginAttempt Nothing
        text = testTextInput
    ck <- combineGUI login text const
    liftIO $ print ck
    return ()

--cursor :: V InputEvent Box
--cursor = Box <$> cursorTransform <*> cursorSize <*> cursorColor
--
--cursorTransform :: V InputEvent Transform
--cursorTransform = pure mempty
--
---- | The size of the cursor is the size of the element at the cursor's
---- current offset.
--cursorSize :: V InputEvent (V2 Float)
--cursorSize = pure $ V2 14 20
--
--cursorColor :: V InputEvent (V4 Float)
--cursorColor = time ~> (purple `alpha`) <$> blink
--
---- | The cursor offset is the multidimensional index of the user's cursor.
---- For text editing this would be the character column x and line row y.
--cursorOffset :: V InputEvent (Int, Int)
--cursorOffset = undefined
--
--blink :: V Float Float
--blink = once 0 `andThenE` tween easeInExpo 0 1 0.3 `andThenE`
--                       tween easeInExpo 1 0 0.8 `andThen` blink
