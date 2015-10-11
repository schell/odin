module Odin.Control.Button (
    buttonGUI,
    button
) where

import Odin.Control.TextInput
import Odin.Data.Common
import Odin.Control.Common
import Control.Varying
import Control.GUI
import Control.Monad.Trans.Reader
import Control.Lens
import Linear
import Gelatin.Core.Color
import Gelatin.Core.Rendering

buttonGUI :: String -> Odin TextInput
buttonGUI str = fst <$> gui (button str) (leftClickInPath $ pure path)
    where btn = input str
          path = textInputPath btn

button :: Monad m => String -> Var (ReaderT Input m) InputEvent TextInput
button = inactiveTextInput . input

input :: String -> TextInput
input str = initialize emptyTextInput $ do
    textInputBox_.boxColor_ .= V4 0.0 0.3 0.1 1
    textInputBox_.boxSize_ .= V2 200 32
    textInputTransform_ .= Transform (V2 10 10) 1 0
    textInputText_.plainTxtString_ .= str
    textInputText_.plainTxtColor_ .= white
