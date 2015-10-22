module Odin.Control.TextForm where

import Odin.GUI
import Odin.Data.Common
import Odin.Control.Common
import Odin.Control.TextField
import Odin.Control.TextInput
import Control.Varying
import Control.Monad.Trans
import Control.Lens
import Gelatin.Core.Rendering
import Linear

textForm :: Varying Transform -> [String] -> String -> Odin TextForm
textForm vt labels btnLabel = do
    btnSize <- lift $ textSize btnLabel
    let initField s i = mutate defaultTextField $ do
                            textFieldLabel._2.plainTextString .= s
                            textFieldInput.textInputText
                            textFieldError._2.plainTextString .= ""
        fields = zipWith initField labels [0..]
        btn    = mutate defaultTextInput $ do
                     textInputText._2.plainTextString .= btnLabel
                     textInputBox._2.boxSize .= btnSize
                     textInputBox._2.boxColor .= V4 0 1 0 1
    (_,V2 x y) <- gui (pure fields) leftClickPos
    return $ TextForm fields btn 0
