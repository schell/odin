module Odin.GUI.Button where

import           Gelatin.Core.Font
import           Gelatin.Core.Color
import           Gelatin.Core.Fill
import           Gelatin.Picture
import           Data.Hashable
import           Linear

data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown

data Button = Button { btnText      :: String
                     , btnFont      :: FontData
                     , btnPointSize :: Float
                     , btnState     :: ButtonState
                     }

paintButton :: Button -> Picture a ()
paintButton btn = do
  let textColor = case btnState btn of
                    ButtonStateUp -> white `alpha` 0.7
                    ButtonStateOver -> white `alpha` 0.8
                    ButtonStateDown -> white
      text = withLetters $ filled (Name $ hash textColor) (btnFont btn)
               72 (btnPointSize btn) (btnText btn) $ solid textColor
      size@(V2 _ th) = pictureSize text
      padding = 4
      inc = 2 * padding
      bg = withColor $ rectangle 0 (size + inc) $ const $ V4 0.3 0.3 0.3 1
  bg
  move (V2 0 th + padding) text

buttonBounds :: Button -> (V2 Float, V2 Float)
buttonBounds = pictureBounds . paintButton
