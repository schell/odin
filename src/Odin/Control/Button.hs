module Odin.Control.Button (
    button,
    vbutton
) where

import Odin.Control.TextInput
import Odin.Data.Common
import Odin.Control.Common
import Odin.GUI
import Control.Lens
import Linear
import Gelatin.Core.Color
import Gelatin.Core.Rendering

button :: Varying Transform -> String -> Odin ()
button vt str = do
    let btn = vbutton vt str
    () <$ gui btn (clickInTextInput btn)

vbutton :: Varying Transform -> String -> Varying TextInput
vbutton vt = inactiveTextInput vt . input

input :: String -> TextInput
input str = initialize emptyTextInput $ do
    textInputBox._2.boxColor .= V4 0.0 0.3 0.1 1
    textInputBox._2.boxSize .= V2 200 32
    textInputText._2.plainTextString .= str
    textInputText._2.plainTextColor .= white
