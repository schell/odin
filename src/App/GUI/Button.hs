module App.GUI.Button where

import           Gelatin.Core.Font
import           Gelatin.Core.Color
import           Gelatin.Core.Fill
import           Gelatin.Picture
import           Gelatin.SDL2
import           SDL
import           SDL.Raw.Enum
import           Data.Hashable
import           Control.Arrow
import           Control.Varying
import           Control.Monad.Trans.RWS.Strict

import           App.Control.Monad
import           App.Control.FRP

data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown
                 deriving (Show, Eq, Enum, Bounded)

data Button = Button { btnState     :: ButtonState
                     , btnText      :: String
                     , btnFont      :: FontData
                     , btnPointSize :: Float
                     }

type ButtonPainter a = Button -> Picture a ()

paintButton :: Button -> Picture a ()
paintButton btn = do
  let (textColor,bgColor) =
        case btnState btn of
          ButtonStateUp -> (white `alpha` 0.7, V4 0.3 0.3 0.3 1)
          ButtonStateOver -> (canary, V4 0.3 0.3 0.3 1)
          ButtonStateDown -> (white, V4 0.2 0.2 0.2 1)
      text = withLetters $ filled (Name $ hash textColor) (btnFont btn)
               72 (btnPointSize btn) (btnText btn) $ solid textColor
      size@(V2 _ th) = pictureSize text
      padding = 4
      inc = 2 * padding
      bg = withColor $ rectangle 0 (size + inc) $ const bgColor
  bg
  move (V2 0 th + padding) text

btnBounds :: Button -> (V2 Float, V2 Float)
btnBounds = pictureBounds . paintButton

btnSize :: Button -> V2 Float
btnSize = pictureSize . paintButton

buttonWith :: Monad m
       => ButtonPainter a -> Button -> V2 Float
       -> SplineT AppEvent (Picture a ()) (RWST ReadData [Action] s m) Button
buttonWith pnt btn p
  | btnState btn == ButtonStateUp = do

    let mouseOver = mouseIsOver $ first (p+) $ second (p+) $ btnBounds btn
    pure (move p $ pnt btn) `_untilEvent_` mouseOver
    pushCursor CursorHand
    buttonWith pnt btn{ btnState = ButtonStateOver } p

  | btnState btn == ButtonStateOver = do

    let mouseDown = mouseButtonEvent ButtonLeft Pressed
        mouseOut  = mouseIsOut $ first (p+) $ second (p+) $ btnBounds btn
        endEvent  = eitherE mouseOut mouseDown
    e <- pure (move p $ pnt btn) `_untilEvent` endEvent
    case e of
      Left _ -> do popCursor CursorHand
                   buttonWith pnt btn{ btnState = ButtonStateUp } p
      Right _ -> buttonWith pnt btn{ btnState = ButtonStateDown } p

  | btnState btn == ButtonStateDown = do
    let mouseUp  = mouseButtonEvent ButtonLeft Released
        mouseOut = mouseIsOut $ first (p+) $ second (p+) $ btnBounds btn
        endEvent = eitherE mouseOut mouseUp
        btn1 = btn{ btnState = ButtonStateOver }
    e <- pure (move p $ pnt btn) `_untilEvent` endEvent
    case e of
      Left _ -> do popCursor CursorHand
                   buttonWith pnt btn{btnState = ButtonStateUp} p
      Right _ -> do step $ move p $ pnt btn1
                    return btn1
  | otherwise = return btn

button :: Button -> V2 Float -> AppSequence (Picture a ()) Button
button = buttonWith paintButton
