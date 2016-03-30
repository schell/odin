{-# LANGUAGE PatternGuards #-}
module App.Control.FRP where

import           Control.Varying
import           Control.Monad (join)
import           Control.Monad.Trans.RWS.Strict (asks, RWST)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Concurrent.Async
import           Data.Maybe (isJust)
import           Data.Text (Text)
import           Linear
import           Gelatin.SDL2
import           SDL hiding (Event, windowSize)
import           Codec.Picture (readImage)

import App.Control.Monad

asyncEvent :: MonadIO m => IO b -> VarT m a (Event (Either String b))
asyncEvent f = flip resultStream () $ do
  a <- liftIO $ async f
  let ev = varM (const $ liftIO $ poll a) ~> onJust
  meb <- pure () `_untilEvent` ev
  return $ case meb of
    Left exc -> Left $ show exc
    Right b -> Right b

reqImage :: FilePath -> AppSignal (Event (Either String (V2 Int, GLuint)))
reqImage fp = flip resultStream () $ do
  eimg <- pure () `_untilEvent` asyncEvent (readImage fp)
  case join eimg of
    Left err  -> return $ Left err
    Right img -> Right <$> liftIO (loadTexture img)

unhandledEvent :: Monad m => VarT m AppEvent (Event EventPayload)
unhandledEvent = var f ~> onJust
  where f (AppEventOther ev) = Just ev
        f _ = Nothing

anyEvent :: Monad m => VarT m AppEvent (Event AppEvent)
anyEvent = var f ~> onJust
  where f AppEventNone = Nothing
        f x = Just x

dropEvent :: Monad m => VarT m AppEvent (Event FilePath)
dropEvent = var f ~> onJust
  where f (AppEventDrop fp) = Just fp
        f _ = Nothing

--------------------------------------------------------------------------------
-- Keyboard stuff
--------------------------------------------------------------------------------
-- | When text input is enabled, this event stream will produce text typed by
-- the user. Use 'startEditingText' and 'stopEditingText' to enable and disable
-- the triggering of this stream.
--
-- The monad is parameterized with 'RWST r [Action] s m'. This is done to ensure
-- that the underlying machinery for triggering text input mode is avilable.
textInputEvent :: Monad m => VarT (RWST r [Action] s m) AppEvent (Event Text)
textInputEvent = var f ~> onJust
  where f (AppEventTextInput txt) = Just txt
        f _ = Nothing

symIsDel :: Keysym -> Bool
symIsDel sym = key == KeycodeBackspace || key == KeycodeDelete
  where key = keysymKeycode sym

symIsEsc :: Keysym -> Bool
symIsEsc = (== KeycodeEscape) . keysymKeycode

symIsReturn :: Keysym -> Bool
symIsReturn sym = key == KeycodeReturn || key == KeycodeReturn2
    where key = keysymKeycode sym

symIsTab :: Keysym -> Bool
symIsTab = (== KeycodeTab) . keysymKeycode

symToChar :: Keysym -> Maybe Char
symToChar sym
  | keycode sym >= 32 && keycode sym <= 127 = Just $ toEnum $ keycode sym
  | otherwise = Nothing
  where keycode = fromEnum . unwrapKeycode . keysymKeycode

symIsChar :: Keysym -> Bool
symIsChar = isJust . symToChar

anyKeyEvent :: Monad m => VarT m AppEvent (Event (InputMotion, Bool, Keysym))
anyKeyEvent = var f ~> onJust
  where f (AppEventKey a b c) = Just (a,b,c)
        f _ = Nothing

onKeyEvent :: Monad m
           => ((InputMotion, Bool, Keysym) -> Bool)
           -> VarT m AppEvent (Event (InputMotion, Bool, Keysym))
onKeyEvent g = var f ~> onJust
  where f (AppEventKey m r sym) = if g (m,r,sym)
                                  then Just (m,r,sym)
                                  else Nothing
        f _ = Nothing

cancelKeyEvent :: Monad m => VarT m AppEvent (Event ())
cancelKeyEvent = use () $ onKeyEvent $ \(m,_,sym) -> m == Pressed && symIsEsc sym

commitKeyEvent :: Monad m => VarT m AppEvent (Event ())
commitKeyEvent =
  use () $ onKeyEvent $ \(m,_,sym) -> m == Pressed && symIsReturn sym

tabKeyEvent :: Monad m => VarT m AppEvent (Event ())
tabKeyEvent =
  use () $ onKeyEvent $ \(m,_,sym) -> m == Pressed && symIsTab sym

deleteKeyEvent :: Monad m => VarT m AppEvent (Event ())
deleteKeyEvent =
  use () $ onKeyEvent $ \(m,_,sym) -> m == Pressed && symIsDel sym
--------------------------------------------------------------------------------
-- Mouse/Cursor stuff
--------------------------------------------------------------------------------
mouseWheelEvent :: Monad m => VarT m AppEvent (Event (V2 Int))
mouseWheelEvent = var f ~> onJust
  where f (AppEventWheel v) = Just v
        f _ = Nothing

anyMouseButtonEvent :: Monad m => VarT m AppEvent (Event (MouseButton, InputMotion, V2 Int))
anyMouseButtonEvent = var f ~> onJust
  where f (AppEventMouseButton btn action v) = Just (btn,action,v)
        f _ = Nothing

mouseButtonEvent :: Monad m
           => MouseButton -> InputMotion -> VarT m AppEvent (Event (V2 Int))
mouseButtonEvent btn action = anyMouseButtonEvent ~> var f ~> onJust
  where f (Event (btn0,action0,v)) =
          if btn0 == btn && action0 == action
          then Just v
          else Nothing
        f _ = Nothing

-- | A stream of mouse motion events. `fst` contains the new mouse position
-- while `snd` contains the relative vector since the last event.
mouseMotionEvent :: Monad m => VarT m AppEvent (Event (V2 Int, V2 Int))
mouseMotionEvent = var f ~> onJust
  where f (AppEventMouseMotion p v) = Just (p,v)
        f _ = Nothing

-- | A stream of relative motion of th mouse.
mouseMoveEvent :: Monad m => VarT m AppEvent (Event (V2 Float))
mouseMoveEvent = fmap (fmap fromIntegral <$> snd) <$> mouseMotionEvent

-- | The absolute mouse position over time.
mousePosition :: (Monad m, Monoid w) => VarT (RWST ReadData w s m) a (V2 Int)
mousePosition = varM $ const getMousePosition

mouseIsOver :: (Monad m, Monoid w)
            => (V2 Float, V2 Float) -> VarT (RWST ReadData w s m) a (Event ())
mouseIsOver bounds =
  mousePosition ~> var ((`pointInBounds` bounds) . fmap fromIntegral) ~> onTrue

mouseIsOut :: (Monad m, Monoid w)
            => (V2 Float, V2 Float) -> VarT (RWST ReadData w s m) a (Event ())
mouseIsOut bounds =
  mousePosition ~> var (not . (`pointInBounds` bounds) . fmap fromIntegral)
                ~> onTrue

windowSize :: VarT Effect a (V2 Int)
windowSize = varM $ const $ asks rdWindowSize

halfWindowSize :: VarT Effect a (V2 Float)
halfWindowSize = (fmap fromIntegral <$> windowSize) / 2

time :: Monad m => VarT m AppEvent Float
time = var f
  where f (AppEventTime dt) = dt
        f _ = 0

frames :: Monad m => VarT m AppEvent Int
frames = var f ~> accumulate (+) 0
  where f AppEventFrame = 1
        f _ = 0

typingBuffer :: Monad m => VarT m AppEvent String
typingBuffer = typingBufferOn "" $ always ()

typingBufferOn :: Monad m
               => String -> VarT m AppEvent (Event a)
               -> VarT m AppEvent String
typingBufferOn ss on =
  ((>>) <$> on <*> anyKeyEvent) ~> foldStream f ss
    where f s (Pressed, _, sym)
            | Just c <- symToChar sym = s ++ [c]
            | symIsDel sym = if null s then [] else init s
          f s _ = s

--tabCount :: Monad m => Var m AppEvent Int
--tabCount = ((1 <$) <$> tab) ~> foldStream (+) 0
--
--tab :: Monad m => Var m AppEvent (Event ())
--tab = f <$> keyInput
--    where f (Event (KeyMod Key'Tab)) = Event ()
--                    f _ = NoEvent
--
--enter :: Monad m => Var m AppEvent (Event ())
--enter = f <$> keyInput
--    where f (Event (KeyMod Key'Enter)) = Event ()
--                    f _ = NoEvent
--
