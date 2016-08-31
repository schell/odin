{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
import           Gelatin.SDL2
import           Gelatin.FreeType2
import           SDL hiding (time)
import           Odin.Core
import           Halive.Utils
import           Demos.Utils
import           Control.Lens
import           Odin.GUI.Button
import           Odin.GUI.Styles
--import System.Remote.Monitoring
--------------------------------------------------------------------------------
-- Immediate mode GUI experiments
--------------------------------------------------------------------------------
runFrame :: Update a -> StateT Frame IO a
runFrame f = do
  use rez >>= io . clearFrame
  tickTime
  tickEvents
  e <- runEventT f
  use window >>= io . updateWindowSDL2
  case e of
    Left g   -> runFrame g
    Right a  -> return a

mainFrame :: Atlas -> Slot Button -> Int -> Update ()
mainFrame atlas button i = do
  waitUntil ((== ButtonStateClicked) <$> renderButton button [])
  newbutton <- allocButton atlas ("Clicked " ++ show i) buttonPainter
  freeButton button
  next $ mainFrame atlas newbutton $ i + 1

setupFrame :: FilePath -> Update ()
setupFrame font = do
  Just atlas <- allocAtlas font (PixelSize 16 16) asciiChars
  button     <- allocButton atlas "Button" buttonPainter
  mainFrame atlas button 0

main :: IO ()
main = do
  comicFont <- getFontPath "KMKDSP__.ttf"
  (rz,win)  <- reacquire 0 $ startupSDL2Backend 800 600 "Immediate mode" True
  t         <- newTime
  void $ flip runStateT (Frame t [] 0 win rz) $ runFrame $ setupFrame comicFont
  putStrLn "done!"
