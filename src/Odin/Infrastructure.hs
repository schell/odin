-- |
--   Module:     Main
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--
module Odin.Infrastructure (
    UserInput(..),
    Effect,
    Pic,
    Network,
    cursorMoved,
    cursorPosition,
    time,
    logStr,
    getWindowSize,
    windowSize,
    run
) where

import Control.Varying
import Gelatin.SDL2 hiding (Event, time, windowSize)
import qualified SDL
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.STM
import Control.Monad
import Control.Concurrent.STM.TVar
import Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.IO.Class
import Data.Bits ((.|.))
import System.Exit
import Linear.Affine (Point(..))
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data UserInput = InputUnknown String
               | InputTime Float
               | InputCursor Float Float
               | InputWindowSize Int Int
               | InputWindowClosed
               | InputKey { keyMotion :: InputMotion 
                          , keyRepeat :: Bool 
                          , keySym    :: Keysym
                          }
               deriving (Show)

data OutputEvent = OutputEventUnknown String
                 | OutputNeedsUpdate
                 | OutputPutStrLn String
                 | OutputQuit
                 deriving (Ord, Eq)

type WrapIO = IdentityT IO
type Effect = RWST Window [OutputEvent] () WrapIO
type Pic = Picture ()
type Network = VarT Effect UserInput Pic
data AppData = AppData { appNetwork :: Network
                       , appCache   :: Cache IO Transform
                       , appEvents  :: [UserInput]
                       }
--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
queryWindowSize :: Effect (V2 Int)
queryWindowSize = do
    window <- ask 
    size   <- liftIO $ SDL.get $ SDL.windowSize window
    return $ fromIntegral <$> size 
--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------
renderFrame :: Window -> Rez -> Cache IO Transform -> Pic 
            -> IO (Cache IO Transform)
renderFrame window rez cache pic = do
  (fbw,fbh) <- ctxFramebufferSize $ rezContext rez 
  glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  newCache <- renderPrims rez cache $ toPaintedPrimitives pic
  glSwapWindow window 
  return newCache 
--------------------------------------------------------------------------------
-- Network Streams
--------------------------------------------------------------------------------
cursorMoved :: (Applicative m, Monad m) => VarT m UserInput (Event (V2 Float))
cursorMoved = var f ~> onJust
    where f (InputCursor x y) = Just $ V2 x y
          f _ = Nothing

cursorPosition :: (Applicative m, Monad m) => VarT m UserInput (V2 Float)
cursorPosition = cursorMoved ~> foldStream (\_ v -> v) (-1)

timeUpdated :: (Applicative m, Monad m) => VarT m UserInput (Event Float)
timeUpdated = var f ~> onJust
    where f (InputTime t) = Just t
          f _ = Nothing

deltas :: (Applicative m, Monad m) => VarT m UserInput Float
deltas = 0 `orE` timeUpdated

requestUpdate :: VarT Effect a a
requestUpdate = varM $ \input -> do
    tell [OutputNeedsUpdate] 
    return input

time :: VarT Effect UserInput Float
time = deltas ~> requestUpdate

windowSize :: VarT Effect a (V2 Int)
windowSize = varM $ const queryWindowSize
--------------------------------------------------------------------------------
-- Network Splines
--------------------------------------------------------------------------------
logStr :: Monad m => String -> SplineT a b (RWST r [OutputEvent] s m) ()
logStr = lift . tell . (:[]) . OutputPutStrLn

getWindowSize :: SplineT a b Effect (V2 Int)
getWindowSize = lift queryWindowSize
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
oneFrame :: Float
oneFrame = 1/30 

fevent :: SDL.Event -> [UserInput]
fevent (SDL.Event _ 
        (MouseMotionEvent 
         (MouseMotionEventData _ _ _ (P (V2 x y)) _))) = 
    [InputCursor (fromIntegral x) (fromIntegral y)]
fevent (SDL.Event _ 
        (WindowResizedEvent 
         (WindowResizedEventData _ (V2 w h)))) =
    [InputWindowSize (fromIntegral w) (fromIntegral h)]
fevent (SDL.Event _ 
        (WindowClosedEvent 
         (WindowClosedEventData _))) =
    [InputWindowClosed]
fevent (SDL.Event _
        (KeyboardEvent
         (KeyboardEventData _ m r k))) = 
    [InputKey m r k]
fevent _ = []

push :: TVar AppData -> UserInput -> IO ()
push tvar input = atomically $ modifyTVar' tvar $ \app -> 
    app{ appEvents = appEvents app ++ [input] }

isQuit :: Keysym -> Bool
isQuit (Keysym (Scancode 20) (Keycode 113) m) = any ($ m) 
    [ keyModifierLeftCtrl
    , keyModifierRightCtrl
    , keyModifierLeftGUI
    , keyModifierRightGUI
    ]
isQuit _ = False

addInput :: TVar AppData -> UserInput -> IO ()
addInput _ InputWindowClosed = exitSuccess
addInput tvar i@(InputKey Released False k) = do
    when (isQuit k) exitSuccess
    push tvar i
addInput tvar input = push tvar input

applyOutput :: TVar AppData -> OutputEvent -> IO ()
applyOutput tvar OutputNeedsUpdate = void $ async $ do 
    threadDelay $ round (oneFrame * 1000) 
    push tvar $ InputTime oneFrame 
applyOutput _ (OutputPutStrLn str) = putStrLn str
applyOutput _ OutputQuit = exitSuccess
applyOutput _ _ = return ()

stepOdin :: TVar AppData -> Rez -> Window -> IO ()
stepOdin tvar rez window = do  
    AppData net cache events <- readTVarIO tvar

    let (e,es) = case events of
                     e:es -> (e,es)
                     []   -> (InputUnknown "no events", [])

    ((pic, nextNet), (), outs) <- runIdentityT $ runRWST (stepMany es e net) 
                                                         window ()

    newCache <- renderFrame window rez cache pic
    atomically $ writeTVar tvar $ AppData nextNet newCache []

    let needsUpdate = OutputNeedsUpdate `elem` outs
        requests = filter (/= OutputNeedsUpdate) outs 

    mapM_ (applyOutput tvar) requests 
    when needsUpdate $ applyOutput tvar OutputNeedsUpdate

waitOdin :: TVar AppData -> IO () 
waitOdin tvar = do
    pastEvents  <- appEvents <$> readTVarIO tvar
    inputEvents <- pollEvents
    --unless (null inputEvents) $ print inputEvents
    let newEvents = concatMap fevent inputEvents
        allEvents = pastEvents ++ newEvents
    -- only add new events since past events have already been added
    mapM_ (addInput tvar) newEvents 
    -- exit if there are any events, else recurse and poll again
    when (null allEvents) $ do threadDelay 10 
                               waitOdin tvar

run :: VarT Effect UserInput Pic -> IO ()
run myNet = do
    (rez,window) <- startupSDL2Backend 800 600 "Odin" True
    setWindowPosition window $ Absolute $ P $ V2 400 400
    tvar <- atomically $ newTVar AppData{ appNetwork = myNet 
                                        , appCache   = mempty
                                        , appEvents  = []
                                        }
    let loop = stepOdin tvar rez window >> waitOdin tvar >> loop
    loop
