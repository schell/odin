{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Gelatin.SDL2
import           Odin.Core
import           Halive.Utils
import           Demos.Utils
import qualified Demos.Physics1 as Physics1
import qualified Demos.MapCreator as MapCreator

import Odin.Scripts.TextInput

main :: IO ()
main = do
  Right comicFont <- getFont "KMKDSP__.ttf"
  --hackFont  <- getFont "Hack-Regular.ttf"
  (rez,win) <- reacquire 0 $ startupSDL2Backend 800 600 "Entity Sandbox" True
  t         <- newTime
  let sys = (emptySys rez win){ _sysTime = t }
  runSystem sys $ do
    --Physics1.demo comicFont
    MapCreator.demo comicFont
    forever tickSystem
