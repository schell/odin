{-# LANGUAGE PatternGuards #-}
module App.Control.FRP where

import           Control.Varying
import           Control.Monad.Trans.RWS.Strict (asks, RWST)
import           Data.Maybe (isJust)
import           Data.Text (Text)
import           Data.Int (Int32, Int16)
import           Data.Word (Word8)
import           Linear
import           Gelatin.SDL2
import           SDL hiding (Event, windowSize)

import App.Control.Monad

loadImageEvent :: Uid -> AppSignal (Event (Either String (V2 Int, GLuint)))
loadImageEvent (Uid k) = var f ~> onJust
  where f (AppEventLoadImage (Uid t) x) = if k == t then Just x else Nothing
        f _ = Nothing

reqImage :: FilePath -> AppSignal (Event (Either String (V2 Int, GLuint)))
reqImage fp = flip resultStream () $ do
  uid <- loadImageReq fp
  pure () `_untilEvent` loadImageEvent uid

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
-- Joystick stuff
--------------------------------------------------------------------------------
joystickAddedEvent :: Monad m => VarT m AppEvent (Event Int32)
joystickAddedEvent = var f ~> onJust
  where f (AppEventJoystickAdded iid) = Just iid
        f _ = Nothing

joystickRemovedEvent :: Monad m => VarT m AppEvent (Event Int32)
joystickRemovedEvent = var f ~> onJust
  where f (AppEventJoystickRemoved iid) = Just iid
        f _ = Nothing

anyJoystickAxisEvent :: Monad m => VarT m AppEvent (Event (Int32, Word8, Int16))
anyJoystickAxisEvent = var f ~> onJust
  where f (AppEventJoystickAxis jid axis val) = Just (jid, axis, val)
        f _ = Nothing

joystickAxisEvent :: Monad m => Int32 -> Word8 -> VarT m AppEvent (Event Int16)
joystickAxisEvent jid axis = var f ~> onJust
  where f (AppEventJoystickAxis kid axis1 val) = if (jid,axis) == (kid,axis1)
                                                   then Just val
                                                   else Nothing
        f _ = Nothing

joystickAxisPressureEvent :: Monad m
                          => Int32 -> Word8 -> VarT m AppEvent (Event Float)
joystickAxisPressureEvent jid axis =
  fmap f <$> joystickAxisEvent jid axis
    where f i = fromIntegral i / fromIntegral (maxBound :: Int16)

joystickBallEvent :: Monad m
                  => Int32 -> Word8 -> VarT m AppEvent (Event (V2 Int16))
joystickBallEvent jid ball = var f ~> onJust
  where f (AppEventJoystickBall kid ball1 rel) = if (jid,ball) == (kid,ball1)
                                                   then Just rel
                                                   else Nothing
        f _ = Nothing

joystickHatEvent :: Monad m => Int32 -> Word8 -> VarT m AppEvent (Event Word8)
joystickHatEvent jid hat = var f ~> onJust
  where f (AppEventJoystickHat kid hat1 val) = if (jid,hat) == (kid,hat1)
                                                  then Just val
                                                  else Nothing
        f _ = Nothing

joystickButtonEvent :: Monad m
                    => Int32 -> Word8 -> Word8 -> VarT m AppEvent (Event ())
joystickButtonEvent jid btn st =
  var (== AppEventJoystickButton jid btn st) ~> onTrue

anyJoystickButtonEvent :: Monad m
                       => VarT m AppEvent (Event (Int32, Word8, Word8))
anyJoystickButtonEvent = var f ~> onJust
  where f (AppEventJoystickButton jid btn st) = Just (jid, btn, st)
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
