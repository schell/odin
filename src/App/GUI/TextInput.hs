module App.GUI.TextInput where

import           Gelatin.Core.Font
import           Gelatin.Core.Color
import           Gelatin.Core.Fill
import           Gelatin.Picture
import           Gelatin.SDL2
import           SDL hiding (Event, time)
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Arrow
import           Control.Monad
import           Control.Varying
import           Control.Monad.Trans.RWS.Strict

import           App.Control.Monad
import           App.Control.FRP

data TextInputState = TextInputStateUp
                    | TextInputStateOver
                    | TextInputStateDown
                    | TextInputStateEditing
                    deriving (Show, Eq, Enum, Bounded)

data TextInput = TextInput { txtnState     :: TextInputState
                           , txtnText      :: Text
                           , txtnFont      :: FontData
                           , txtnPointSize :: Float
                           , txtnInterval  :: Float
                           }

type TextInputPainter a = TextInput -> Picture a ()

paintTextInput :: TextInput -> Picture a ()
paintTextInput txt = do
  let (textColor,bgColor,lnColor) =
        case txtnState txt of
          TextInputStateUp -> (white, V4 1 1 1 0, white `alpha` 0.4)
          TextInputStateOver -> (white, V4 1 1 1 0, white `alpha` 0.8)
          TextInputStateDown -> (white, V4 0.3 0.3 0.3 1, white)
          TextInputStateEditing -> (canary, V4 0.2 0.2 0.2 1, white `alpha` 0.8)
      px = txtnPointSize txt
      text = withLetters $ filled (txtnFont txt) 72 px (T.unpack $ txtnText txt) $
               solid textColor
      leaderInc = if hasLeader then V2 inc 0 else 0
      endSpaces = T.length $ T.takeWhile (== ' ') $ T.reverse $ txtnText txt
      spaceInc = V2 (px/2) 0 ^* fromIntegral endSpaces
      size@(V2 tw th) = sum [pictureSize text, leaderInc, spaceInc]
      bar = withColor $ rectangle (V2 0 0) (V2 padding th) $
              const $ textColor `alpha` txtnInterval txt
      hasLeader = txtnState txt == TextInputStateEditing
      padding = 4
      inc = 2* padding
      bg = withColor $ rectangle 0 (size + V2 inc inc) $ const bgColor
      outline = withStroke [StrokeWidth 3, StrokeFeather 1] $
        lineStart (0, lnColor) $ do lineTo (V2 (tw + inc) 0, lnColor)
                                    lineTo (V2 (tw + inc) (th + inc), lnColor)
                                    lineTo (V2 0 (th + inc), lnColor)
                                    lineTo (0, lnColor)
  bg
  move (V2 0 th + V2 padding padding) text
  when hasLeader $ move (V2 tw padding) bar
  outline

txtnBounds :: TextInput -> BBox
txtnBounds = pictureBounds . paintTextInput

txtnSize :: TextInput -> V2 Float
txtnSize = pictureSize . paintTextInput

data TextInputEvent = TextInputEventDelete
                    | TextInputEventCancel
                    | TextInputEventCommit
                    | TextInputEventText Text
                    deriving (Show, Eq)

textInputTextEvent :: Monad m
                   => VarT (RWST r [Action] s m) AppEvent (Event TextInputEvent)
textInputTextEvent = (TextInputEventText <$>) <$> textInputEvent

textInputDeleteEvent :: Monad m => VarT m AppEvent (Event TextInputEvent)
textInputDeleteEvent = (TextInputEventDelete <$) <$> deleteKeyEvent

textInputCancelEvent :: Monad m => VarT m AppEvent (Event TextInputEvent)
textInputCancelEvent = (TextInputEventCancel <$) <$> cancelKeyEvent

textInputCommitEvent :: Monad m => VarT m AppEvent (Event TextInputEvent)
textInputCommitEvent = (TextInputEventCommit <$) <$> commitKeyEvent

anyTextInputEvent :: Monad m
                  => VarT (RWST r [Action] s m) AppEvent (Event TextInputEvent)
anyTextInputEvent = anyE [ textInputDeleteEvent
                         , textInputTextEvent
                         , textInputCancelEvent
                         , textInputCommitEvent
                         ]

textInputWith :: Monad m
          => TextInputPainter a -> TextInput -> V2 Float
          -> SplineT AppEvent (Picture a ()) (RWST ReadData [Action] s m) TextInput
textInputWith pnt txt p
  | txtnState txt == TextInputStateUp = do

    let mouseOver = mouseIsOver $ first (p+) $ second (p+) $ txtnBounds txt
    pure (move p $ pnt txt) `_untilEvent_` mouseOver
    pushCursor CursorHand
    textInputWith pnt txt{ txtnState = TextInputStateOver } p

  | txtnState txt == TextInputStateOver = do

    let mouseDown = mouseButtonEvent ButtonLeft Pressed
        mouseOut  = mouseIsOut $ first (p+) $ second (p+) $ txtnBounds txt
        endEvent  = eitherE mouseOut mouseDown
    e <- pure (move p $ pnt txt) `_untilEvent` endEvent
    case e of
      Left _ -> do popCursor CursorHand
                   textInputWith pnt txt{ txtnState = TextInputStateUp } p
      Right _ -> textInputWith pnt txt{ txtnState = TextInputStateDown } p

  | txtnState txt == TextInputStateDown = do
    let mouseUp  = mouseButtonEvent ButtonLeft Released
        mouseOut = mouseIsOut $ first (p+) $ second (p+) $ txtnBounds txt
        endEvent = eitherE mouseOut mouseUp
    e <- pure (move p $ pnt txt) `_untilEvent` endEvent
    case e of
      Left _ -> do popCursor CursorHand
                   textInputWith pnt txt{txtnState = TextInputStateUp} p
      Right _ -> do startTextEditing
                    textInputWith pnt txt{ txtnState = TextInputStateEditing } p

  | txtnState txt == TextInputStateEditing = do
    let mouseDown = mouseButtonEvent ButtonLeft Pressed
        endEvent = eitherE mouseDown anyTextInputEvent
        bounds = first (p+) $ second (p+) $ txtnBounds txt
        text = txtnText txt
        --attack  = do void $ tween easeOutExpo 0 1 0.2
        --             void $ constant 1 0.5
        --             decay
        --decay = do void $ tween easeInExpo 1 0 0.3
        --           void $ tween easeOutExpo 0 1 0.3
        --           decay
        --interval = time ~> outputStream attack 0
        paintTxt t = move p $ pnt txt{txtnInterval = t}
    (pic,e) <- pure (paintTxt 1) `untilEvent` endEvent
    step pic
    case e of
      Left v -> do
        let v1 = fromIntegral <$> v
            hit = pointInBounds v1 bounds
        if hit
          then textInputWith pnt txt{ txtnState = TextInputStateEditing } p
          else do popCursor CursorHand
                  textInputWith pnt txt{ txtnState = TextInputStateUp } p
      Right TextInputEventDelete ->
        let text1 = if T.null text then T.empty else T.init text
        in textInputWith pnt txt{txtnText = text1 } p
      Right (TextInputEventText t) ->
        textInputWith pnt txt{txtnText = text `T.append` t} p
      Right TextInputEventCancel ->
        textInputWith pnt txt{txtnState = TextInputStateOver} p
      Right TextInputEventCommit ->
        textInputWith pnt txt{txtnState = TextInputStateOver} p
    --  Right v -> do
      | otherwise = return txt

textInput :: TextInput -> V2 Float -> AppSequence (Picture a ()) TextInput
textInput = textInputWith paintTextInput
