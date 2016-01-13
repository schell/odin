{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Odin.Control.Common where

import Jello.GLFW
import Data.Time.Clock
import Control.Concurrent.Async
import Control.Exception (SomeException)
import Control.Monad.State
import qualified Data.IntMap as IM

type UserData = Cache IO Transform
type App a = Spline InputEvent (Picture Font ()) (RWST (ReadData UserData) () () IO) a

data KeyInput = KeyChar Char
              | KeyMod Int
              deriving (Show, Eq)

initialize :: s -> State s () -> s
initialize s f = execState f s

mutate :: s -> State s () -> s
mutate = initialize

typingBuffer :: Monad m => Var m InputEvent String
typingBuffer = typingBufferOn "" $ always ()

typingBufferOn :: Monad m
               => String -> Var m InputEvent (Event a)
               -> Var m InputEvent String
typingBufferOn ss on =
    ((>>) <$> on <*> keyInput) ~> foldStream f ss
    where f s k
           | KeyChar c <- k = s ++ [c]
           | KeyMod key <- k
           , True <- key == deleteKey = if null s then [] else init s
           | KeyMod key <- k
           , True <- key == backspaceKey = f s (KeyMod deleteKey)
           | otherwise = s

tabCount :: Monad m => Var m InputEvent Int
tabCount = ((1 <$) <$> tab) ~> foldStream (+) 0

tab :: Monad m => Var m InputEvent (Event ())
tab = f <$> keyInput
    where f e
           | (Event (KeyMod k)) <- e
           , True <- tabKey == k = Event ()
           | otherwise = NoEvent

enter :: Monad m => Var m InputEvent (Event ())
enter = f <$> keyInput
    where f e
           | (Event (KeyMod k)) <- e
           , True <- enterKey == k = Event ()
           | otherwise = NoEvent

time :: MonadIO m => Var m a Float
time = delta (liftIO getCurrentTime) (\a b -> realToFrac $ diffUTCTime a b)

keyInput :: Monad m => Var m InputEvent (Event KeyInput)
keyInput = var check ~> onJust
    where check e
            | (CharEvent c) <- e
            = Just $ KeyChar c
            | (KeyEvent k _ ks _) <- e
            , True <- keyIsPressed ks
            = Just $ KeyMod k
            | otherwise = Nothing

currentNumberOfRenderers :: (Monad m, Monoid w)
                         => Var (RWST (ReadData UserData) w s m) a Int
currentNumberOfRenderers = varM $ const $ asks (IM.size . readUserData)

--untilEvent :: Monad m
--           => Var m a b -> (Var m a (Event c), c -> b -> Var m a b) -> Var m a b
--untilEvent va (ve, f) = Var $ \a -> do
--    (b, va') <- runVar va a
--    (e, ve') <- runVar ve a
--    case e of
--       Event c -> runVar (f c b) a
--       NoEvent -> return (b, untilEvent va' (ve', f))

windowSize :: (Monad m, Monoid w) => Var (RWST (ReadData u) w s m) i (V2 Float)
windowSize = varM $ const $ asks readWindowSize

cursorPos :: (Monad m, Monoid w) => Var (RWST (ReadData u) w s m) i (V2 Float)
cursorPos = varM $ const $ asks readCursorPos

--isCursorInPath :: (Monad m, Monoid w)
--               => Var (RWST ReadData w s m) i Path
--               -> Var (RWST ReadData w s m) i Bool
--isCursorInPath vpath = pointInside <$> cursorPos <*> vpath
--
--cursorInPath :: (Monad m, Monoid w)
--             => Var (RWST ReadData w s m) i Path
--             -> Var (RWST ReadData w s m) i (Event ())
--cursorInPath vpath = isCursorInPath vpath ~> onTrue
--
--cursorNotInPath :: (Monad m, Monoid w)
--                => Var (RWST ReadData w s m) i Path
--                -> Var (RWST ReadData w s m) i (Event ())
--cursorNotInPath vpath = (not <$> isCursorInPath vpath) ~> onTrue

--leftClickInPath :: (Monad m, Monoid w)
--                => Var (RWST ReadData w s m) InputEvent Path
--                -> Var (RWST ReadData w s m) InputEvent (Event ())
--leftClickInPath vpath = (f <$> leftClickPos <*> vpath) ~> onTrue
--    where f (Event v) path = v `pointInside` path
--          f _ _ = False
--
--leftClickOutPath :: (Monad m, Monoid w)
--                 => Var (RWST ReadData w s m) InputEvent Path
--                 -> Var (RWST ReadData w s m) InputEvent (Event ())
--leftClickOutPath vpath = (f <$> leftClickPos <*> vpath) ~> onTrue
--    where f (Event v) path = not $ pointInside v path
--          f _ _ = False

leftClickPos :: (Monad m, Monoid w)
             => Var (RWST (ReadData u) w s m) InputEvent (Event (V2 Float))
leftClickPos = (<$) <$> cursorPos <*> leftClick

leftClick :: Monad m => Var m InputEvent (Event ())
leftClick = var f ~> onTrue
    where f (MouseButtonEvent 0 False _) = True
          f _ = False

cursorMoved :: Monad m => Var m InputEvent (Event (V2 Float))
cursorMoved = var f ~> onJust
    where f (CursorMoveEvent x y) = Just $ realToFrac <$> V2 x y
          f _ = Nothing

varPoll :: MonadIO m => Async b -> Var m a (Event (Either SomeException b))
varPoll a = Var $ \_ -> do
    mea <- liftIO $ poll a
    case mea of
        Nothing -> -- thread still running, poll next frame
                   return (NoEvent, varPoll a)
        Just ea -> return (Event ea, always ea)

newFontCacheLoaded :: MonadIO m
                   => Var m a (Event (Either SomeException FontCache))
newFontCacheLoaded = Var $ \_ -> do
   afc <- liftIO $ async buildCache
   return (NoEvent, varPoll afc)

--textSize :: (Monad m, Monoid w) => String -> (RWST ReadData w s m) (V2 Float)
--textSize s = do
--    dpi <- asks _readDpi
--    rez <- asks _readResources
--    let BoundingBox _ _ w h _ = stringBoundingBox (rezFont rez) dpi sz s
--        sz = pixelSizeInPointAtDpi 16 dpi
--    return $ V2 w h
