-- |
--   Module:     Main
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--
module Odin.Infrastructure where

import Control.Varying
import Gelatin.SDL2 hiding (Event, time)
import qualified SDL
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad.Trans.Writer.Strict
import Control.Monad
import Data.Bits ((.|.))
import Data.Time.Clock
import qualified Data.Set as S
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

instance Monoid UserInput where
    mappend a (InputUnknown _) = a
    mappend _ b = b
    mempty = InputUnknown ""

data OutputEvent = OutputEventUnknown String
                 | OutputNeedsUpdate
                 deriving (Ord, Eq)

type Effect = WriterT [OutputEvent] IO
type Pic = Picture Font ()
type Network = VarT Effect UserInput Pic
data AppData = AppData { appNetwork :: Network
                       , appCache   :: Cache IO Transform
                       , appEvents  :: [UserInput]
                       }
--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------
renderFrame :: Window -> Rez -> Cache IO Transform -> Pic 
            -> IO (Cache IO Transform)
renderFrame window rez cache pic = do
  (fbw,fbh) <- ctxFramebufferSize $ rezContext rez 
  glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  newCache <- renderPrims rez cache $ pictureToR2Primitives pic
  glSwapWindow window 
  return newCache 
--------------------------------------------------------------------------------
-- Network
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
applyOutput _ _ = return ()

stepOdin :: TVar AppData -> Rez -> Window -> IO ()
stepOdin tvar rez window = do  
    AppData net cache events <- readTVarIO tvar
    let evs = events
    ((pic, nextNet), outs) <- runWriterT $ stepMany evs net 
    newCache <- renderFrame window rez cache pic
    atomically $ writeTVar tvar $ AppData nextNet newCache []
    let requests = S.toList $ foldr S.insert S.empty outs
    mapM_ (applyOutput tvar) requests 

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
    (rez,window) <- startupSDL2Backend 800 600 "Odin"
    setWindowPosition window $ Absolute $ P $ V2 400 400
    tvar <- atomically $ newTVar AppData{ appNetwork = myNet 
                                        , appCache   = mempty
                                        , appEvents  = []
                                        }
    let loop = stepOdin tvar rez window >> waitOdin tvar >> loop
    loop
