{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Gelatin.SDL2
import           Odin.Core
import           Halive.Utils
import           Demos.Utils
import qualified Demos.Physics1 as Physics1

import Odin.Scripts.TextInput

main :: IO ()
main = do
  Right comicFont <- getFont "KMKDSP__.ttf"
  --hackFont  <- getFont "Hack-Regular.ttf"
  (rez,win) <- reacquire 0 $ startupSDL2Backend 800 600 "Entity Sandbox" True
  t         <- newTime
  let sys = (emptySystemStep rez win){ sysTime = t }
  runSystem sys $ do
    Physics1.demo comicFont
    let txt = TextInput { txtnFont = comicFont
                        , txtnText = "Blah text..."
                        , txtnPointSize = 16
                        }
    mail <- mailbox
    freshTextInput txt mail #. tfrm (PictureTransform (Transform (V2 4 28) 1 0) 1 1)
    recv mail $ \case
      TextInputStateEdited str -> io $ print str
      _ -> return ()
    forever tickSystem
