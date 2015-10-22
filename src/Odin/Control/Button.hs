module Odin.Control.Button (
    button,
    inactiveButton,
    vbutton
) where

import Odin.Control.TextInput
import Odin.Data.Common
import Odin.Control.Common
import Odin.GUI
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Varying
import Linear
import Gelatin.Core.Color
import Gelatin.Core.Rendering

buttonSpline :: String -> SplineOf InputEvent TextInput ()
buttonSpline str = do
    inactiveButton str

inactiveButton :: String -> SplineOf InputEvent TextInput ()
inactiveButton str = do
    size <- lift $ textSize str
    let btn  = mutate (input str) $ textInputBox._2.boxSize .= size
    void $ pure btn `untilEvent` leftClickInPath (pure $ boxPath size)

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
