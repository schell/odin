-- |
--   Module:     Main
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--   The entrypoint to our nifty editor.
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Odin.Graphics.Types
import Odin.Graphics.Renderable
import Odin.Data
import Odin.Control
import Odin.System

import Gelatin.Core.Rendering
import Graphics.UI.GLFW
import Graphics.GL.Core33
import Data.IORef
import Data.Renderable
import Data.Bits
import Control.Concurrent
import Control.Varying
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Fresh
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import System.Exit

main :: IO ()
main = do
    True <- initGelatin
    w    <- newWindow 800 600 "Odin" Nothing Nothing
    setWindowPos w 925 800

    grs <- loadGeomRenderSource
    brs <- loadBezRenderSource
    mrs <- loadMaskRenderSource

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    ref <- newIORef []
    let input i = modifyIORef ref (++ [i])

    setCharCallback w $ Just $ \_ c -> input $
        CharEvent c

    setWindowSizeCallback w $ Just $ \_ w' h' -> input $
        WindowSizeEvent w' h'

    setKeyCallback w $ Just $ \_ k i ks modi -> input $
        KeyEvent k i ks modi

    setMouseButtonCallback w $ Just $ \_ mb mbs modi -> input $
        MouseButtonEvent mb mbs modi

    setCursorPosCallback w $ Just $ \_ x y -> input $
        CursorMoveEvent x y

    setCursorEnterCallback w $ Just $ \_ cs -> input $
        CursorEnterEvent cs

    setScrollCallback w $ Just $ \_ x y -> input $
        ScrollEvent x y

    ---- Not till GLFW-b with 3.1 additions
    --setDropCallback w $ Just $ \_ fs -> do
    --    putStrLn $ "Got files:\n" ++ unlines fs
    --    input $ FileDropEvent fs

    Right hack <- odinFont "Hack-Regular.ttf"
    efawe <- odinFont "fontawesome-webfont.ttf"
    fawe <- case efawe of
        Left str -> putStrLn str >> return hack
        Right fawe -> return fawe
    let rez   = Rez grs brs mrs w hack fawe

    runLift $ flip runReader rez
            $ flip runFresh (Uid 0)
            $ evalState (Attached mempty)
            $ step ref network

step :: MakesScene r
     => IORef [InputEvent] -> Var (Eff r) InputEvent UI
     -> Eff r ()
step ref net = do
    -- Update input events.
    events <- lift $ readIORef ref
    let events' = NoInputEvent : events
    lift $ writeIORef ref []
    -- Step the network
    (el, net') <- stepMany events' net
    rz@Rez{}    <- ask
    Attached rs <- get
    rs' <- lift $ renderFrame rz rs el
    put $ Attached rs'
    step ref net'

stepMany :: (Monad m, Monoid a) => [a] -> Var m a b -> m (b, Var m a b)
stepMany (e:[]) y = runVar y e
stepMany (e:es) y = execVar y e >>= stepMany es
stepMany []     y = runVar y mempty

renderFrame :: Rez -> Cache IO Transform -> Element IO Rez Transform
            -> IO (Cache IO Transform)
renderFrame rz old a = do
    new <- attachIfNeeded rz old a

    (fbw,fbh) <- getFramebufferSize $ rezWindow rz
    glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
    glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

    renderData new a

    pollEvents
    swapBuffers $ rezWindow rz
    shouldClose <- windowShouldClose $ rezWindow rz
    if shouldClose
    then exitSuccess
    else threadDelay 100

    detachUnused new a
