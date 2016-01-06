{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Odin.Control.Common where

import Odin.Data
import Odin.Data.Common
import Servant.Client
import Control.Concurrent.Async
import Gelatin.Core.Rendering
import Graphics.UI.GLFW hiding (init)
import Graphics.Text.TrueType
import Gelatin.Core.Triangulation.Common
import Data.Time.Clock
import qualified Data.Map as M
import Linear
import Odin.GUI
import Control.Concurrent.Async
import Control.Exception (SomeException)
import Control.Varying
import Control.Monad.Trans.RWS.Strict
import Control.Monad.State
import Control.Monad.Trans.Either

type App a = Spline InputEvent (Picture ()) ControlM a

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
    where f s (KeyChar c) = s ++ [c]
          f s (KeyMod Key'Delete) = if null s then [] else init s
          f s (KeyMod Key'Backspace) = f s (KeyMod Key'Delete)
          f s _ = s

tabCount :: Monad m => Var m InputEvent Int
tabCount = ((1 <$) <$> tab) ~> foldStream (+) 0

tab :: Monad m => Var m InputEvent (Event ())
tab = f <$> keyInput
    where f (Event (KeyMod Key'Tab)) = Event ()
          f _ = NoEvent

enter :: Monad m => Var m InputEvent (Event ())
enter = f <$> keyInput
    where f (Event (KeyMod Key'Enter)) = Event ()
          f _ = NoEvent

time :: MonadIO m => Var m a Float
time = delta (liftIO getCurrentTime) (\a b -> realToFrac $ diffUTCTime a b)

keyInput :: Monad m => Var m InputEvent (Event KeyInput)
keyInput = var check ~> onJust
    where check e
            | (CharEvent c) <- e
            = Just $ KeyChar c
            | (KeyEvent k _ ks _) <- e
            , True <- keyPressed ks
            = Just $ KeyMod k
            | otherwise = Nothing
          keyPressed k = k == KeyState'Pressed || k == KeyState'Repeating

--untilEvent :: Monad m
--           => Var m a b -> (Var m a (Event c), c -> b -> Var m a b) -> Var m a b
--untilEvent va (ve, f) = Var $ \a -> do
--    (b, va') <- runVar va a
--    (e, ve') <- runVar ve a
--    case e of
--       Event c -> runVar (f c b) a
--       NoEvent -> return (b, untilEvent va' (ve', f))

caltrops :: (Show b, MonadIO m)
         => EitherT ServantError IO b -> Var m a (Event (Either String b))
caltrops f = Var $ \_ -> do
    e <- liftIO $ async $ runEitherT f
    return (NoEvent, checkAsync e)

checkAsync :: (Show b, MonadIO m)
           => Async (Either ServantError b)
           -> Var m a (Event (Either String b))
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

windowSize :: (Monad m, Monoid w) => Var (RWST ReadData w s m) i (V2 Float)
windowSize = varM $ const $ asks _readWindowSize

cursorPos :: (Monad m, Monoid w) => Var (RWST ReadData w s m) i (V2 Float)
cursorPos = varM $ const $ asks _readCursorPos

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
             => Var (RWST ReadData w s m) InputEvent (Event (V2 Float))
leftClickPos = (<$) <$> cursorPos <*> leftClick

leftClick :: Monad m => Var m InputEvent (Event ())
leftClick = var f ~> onTrue
    where f (MouseButtonEvent MouseButton'1 MouseButtonState'Released _) = True
          f _ = False

cursorMoved :: Monad m => Var m InputEvent (Event (V2 Float))
cursorMoved = var f ~> onJust
    where f (CursorMoveEvent x y) = Just $ realToFrac <$> V2 x y
          f _ = Nothing

varPoll :: MonadIO m => Async b -> Var m a (Event (Either SomeException b))
varPoll a = Var $ \_ -> do
    mea <- liftIO $ poll a
    case mea of
        Nothing -> -- thread still running
                   return (NoEvent, varPoll a)
        Just ea -> return (Event ea, always ea)

newFontCacheLoaded :: MonadIO m
                   => Var m a (Event (Either SomeException FontCache))
newFontCacheLoaded = Var $ \_ -> do
   afc <- liftIO $ async $ buildCache
   return (NoEvent, varPoll afc)

--textSize :: (Monad m, Monoid w) => String -> (RWST ReadData w s m) (V2 Float)
--textSize s = do
--    dpi <- asks _readDpi
--    rez <- asks _readResources
--    let BoundingBox _ _ w h _ = stringBoundingBox (rezFont rez) dpi sz s
--        sz = pixelSizeInPointAtDpi 16 dpi
--    return $ V2 w h
