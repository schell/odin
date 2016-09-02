{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
import           Gelatin.SDL2 (startupSDL2Backend, clearFrame, updateWindowSDL2)
import           Gelatin.FreeType2
import           Odin.Core
import           Halive.Utils
import           Demos.Utils
import           Control.Lens
import           Odin.GUI
--import System.Remote.Monitoring
--------------------------------------------------------------------------------
-- Immediate mode GUI experiments
--------------------------------------------------------------------------------
runFrame :: MonadIO m => UpdateT m a -> StateT Frame m a
runFrame f = do
  use rez >>= io . clearFrame
  tickTime
  tickEvents
  e <- runEventT f
  use window >>= io . updateWindowSDL2
  case e of
    Left g   -> runFrame g
    Right a  -> return a

getTextTask :: MonadIO m => Atlas -> UpdateT m (Maybe String)
getTextTask atlas = do
  withDefaultTextInput  atlas "text"    $ \input  ->
    withDefaultButton   atlas "Okay"    $ \okay   ->
      withDefaultButton atlas "Cancel"  $ \cancel -> do
        V2 _ inputh  <- sizeOfTextInput input
        V2 okayw _   <- sizeOfButton okay
        fix $ \task -> do
          (_, str) <- renderTextInput input []
          okayst   <- renderButton okay   [move $ V2 0 $ inputh + 4]
          cancelst <- renderButton cancel [move $ V2 (okayw + 4) (inputh + 4)
                                          ,multiply $ V4 0.9 0.9 0.9 1
                                          ]
          case (okayst,cancelst) of
            (ButtonStateClicked,_                 ) -> return $ Just str
            (_                 ,ButtonStateClicked) -> return Nothing
            _        -> next task

setupFrame :: MonadIO m => FilePath -> UpdateT m ()
setupFrame font =
  void $ withAtlas font (PixelSize 16 16) asciiChars $ \atlas -> do
    fix $ \task -> do
      getTextTask atlas >>= \case
        Nothing  -> io $ putStrLn "Cancelled"
        Just str -> io $ putStrLn $ "Got text: " ++ show str
      next task

main :: IO ()
main = do
  comicFont <- getFontPath "KMKDSP__.ttf"
  (rz,win)  <- reacquire 0 $ startupSDL2Backend 800 600 "Immediate mode" True
  t         <- newTime
  void $ flip runStateT (Frame t [] 0 win rz) $ runFrame $ setupFrame comicFont
  putStrLn "done!"
