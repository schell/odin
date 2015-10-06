{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Odin.Control.Common where

import Odin.Data
import Servant.Client
import Control.Concurrent.Async
import Graphics.UI.GLFW hiding (init)
import Gelatin.Core.Triangulation.Common
import Data.Time.Clock
import Control.GUI
import Linear
import Control.Monad.State.Strict
import Control.Varying
import Control.Varying.Time hiding (before, after)
import Control.Monad.Trans.Either

type V = Var IO
type Odin = GUI IO InputEvent UI

typingBuffer :: V InputEvent String
typingBuffer = typingBufferOn "" $ always ()

typingBufferOn :: String -> V InputEvent (Event a) -> V InputEvent String
typingBufferOn ss on =
    ((>>) <$> on <*> keyInput) ~> foldStream f ss
    where f s (KeyChar c) = s ++ [c]
          f s (KeyMod Key'Delete) = if null s then [] else init s
          f s (KeyMod Key'Backspace) = f s (KeyMod Key'Delete)
          f s _ = s

tabCount :: V InputEvent Int
tabCount = tabs ~> foldStream (+) 0
    where tabs :: V InputEvent (Event Int)
          tabs = (1 <$) <$> tab

tab :: V InputEvent (Event ())
tab = f <$> keyInput
    where f (Event (KeyMod Key'Tab)) = Event ()
          f _ = NoEvent

enter :: V InputEvent (Event ())
enter = f <$> keyInput
    where f (Event (KeyMod Key'Enter)) = Event ()
          f _ = NoEvent

time :: V a Float
time = delta (liftIO getCurrentTime) (\a b -> realToFrac $ diffUTCTime a b)

keyInput :: V InputEvent (Event KeyInput)
keyInput = var check ~> onJust
    where check e
            | (CharEvent c) <- e
            = Just $ KeyChar c
            | (KeyEvent k _ ks _) <- e
            , True <- keyPressed ks
            = Just $ KeyMod k
            | otherwise = Nothing
          keyPressed k = k == KeyState'Pressed || k == KeyState'Repeating


untilEvent :: Monad m
           => Var m a b -> (Var m a (Event c), c -> b -> Var m a b) -> Var m a b
untilEvent va (ve, f) = Var $ \a -> do
    (b, va') <- runVar va a
    (e, ve') <- runVar ve a
    case e of
       Event c -> runVar (f c b) a
       NoEvent -> return (b, untilEvent va' (ve', f))

caltrops :: Show b => EitherT ServantError IO b -> V a (Event (Either String b))
caltrops f = Var $ \_ -> do
    e <- liftIO $ async $ runEitherT f
    return (NoEvent, checkAsync e)

checkAsync :: Show b => Async (Either ServantError b) -> V a (Event (Either String b))
checkAsync a = Var $ \_ -> do
    r <- liftIO $ poll a
    case r of
        Nothing -> return (NoEvent, checkAsync a)
        Just e  -> case e of
            Left err -> liftIO $ print err >> return (NoEvent, never)
            Right r' -> let x = either (Left . show) Right r'--case r' of
                        in return (Event x, always x)

localhost :: BaseUrl
localhost = BaseUrl Http "localhost" 8081

windowSizeStream :: Monad m => Var m InputEvent (Event (V2 Float))
windowSizeStream = var f ~> onJust
    where f (WindowSizeEvent w h) = Just $ realToFrac <$> V2 w h
          f _ = Nothing

cursorPos :: Monad m => Var m InputEvent (V2 Float)
cursorPos = cursorMoved ~> startingWith 0

isCursorInPath :: Monad m => Var m InputEvent Path -> Var m InputEvent Bool
isCursorInPath vpath = pointInside <$> cursorPos <*> vpath

cursorInPath :: Monad m => Var m InputEvent Path -> Var m InputEvent (Event ())
cursorInPath vpath = isCursorInPath vpath ~> onTrue

cursorNotInPath :: Monad m => Var m InputEvent Path -> Var m InputEvent (Event ())
cursorNotInPath vpath = (not <$> isCursorInPath vpath) ~> onTrue

leftClickInPath :: Monad m => Var m InputEvent Path -> Var m InputEvent (Event ())
leftClickInPath vpath = (f <$> leftClickPos <*> vpath) ~> onTrue
    where f (Event v) path = v `pointInside` path
          f _ _ = False

leftClickOutPath :: Monad m => Var m InputEvent Path -> Var m InputEvent (Event ())
leftClickOutPath vpath = (f <$> leftClickPos <*> vpath) ~> onTrue
    where f (Event v) path = not $ pointInside v path
          f _ _ = False

leftClickPos :: Monad m => Var m InputEvent (Event (V2 Float))
leftClickPos = (<$) <$> cursorPos <*> leftClick

leftClick :: Monad m => Var m InputEvent (Event ())
leftClick = var f ~> onTrue
    where f (MouseButtonEvent MouseButton'1 MouseButtonState'Released _) = True
          f _ = False

cursorMoved :: Monad m => Var m InputEvent (Event (V2 Float))
cursorMoved = var f ~> onJust
    where f (CursorMoveEvent x y) = Just $ realToFrac <$> V2 x y
          f _ = Nothing
