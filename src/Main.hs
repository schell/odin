-- |
--   Module:     Main
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--   The entrypoint to our nifty editor.
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Odin.Control
import Jello.GLFW
import Data.IORef
import qualified Data.IntMap as IM

main :: IO ()
main = do
    ref <- newIORef IM.empty
    ws  <- glfwWorkspace
    -- I want to keep track of the number of renderers stored in the cache
    -- and display that on screen, so I'll save a ref to the cache and then
    -- expose it as a read-only user data.
    let readData = wsReadData ws
        readDataWithUser = readData{ readUserData = IM.empty }
        render rez csh pic = do (rez',csh') <- wsRenderPicture ws rez csh pic
                                writeIORef ref csh'
                                return (rez',csh')
        update _ = readIORef ref

    runWorkspace network ws{ wsRenderPicture = render
                           , wsUpdateUserData = update
                           , wsReadData = readDataWithUser
                           }
