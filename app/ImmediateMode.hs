{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
import           Gelatin.SDL2 (startupSDL2Backend, clearFrame, updateWindowSDL2)
import           Gelatin.FreeType2
import           Odin.Core
import           Odin.GUI
import           Demos.Utils
import           Halive.Utils
import           Control.Lens
--------------------------------------------------------------------------------
-- Immediate mode GUI experiments
--------------------------------------------------------------------------------
getTextTask :: MonadIO m => Atlas -> UpdateT m ()
getTextTask atlas = do
  withDefaultTextInput  atlas "Text"    $ \input  ->
    withDefaultButton   atlas "Okay"    $ \okay   ->
      withDefaultButton atlas "Cancel"  $ \cancel -> do
        V2 _ inputh  <- sizeOfTextInput input
        V2 okayw _   <- sizeOfButton okay
        fix $ \task -> do
          (_, str) <- renderTextInput input []
          okayst   <- renderButton okay   [move 0 $ inputh + 4]
          cancelst <- renderButton cancel [move (okayw + 4) (inputh + 4)
                                          ,multiply 0.9 0.9 0.9 1
                                          ]
          case (okayst,cancelst) of
            (ButtonStateClicked,_                 ) -> return ()
            (_                 ,ButtonStateClicked) -> return ()
            _        -> next task

          withDefaultButton atlas ("Got" ++ show str ++ " ... continue") $ \nextBtn ->
            fix $ \task2 -> do
              renderButton nextBtn [] >>= \case
                ButtonStateClicked -> next task
                _ -> next task2

setupFrame :: MonadIO m => FilePath -> UpdateT m ()
setupFrame font =
  void $ withAtlas font (PixelSize 16 16) asciiChars getTextTask

runFrame :: MonadIO m => UpdateT m a -> StateT Frame m a
runFrame f = do
  use rez >>= io . clearFrame
  tickTime
  tickUIPrepare
  e <- runEventT f
  use window >>= io . updateWindowSDL2
  case e of
    Left g   -> runFrame g
    Right a  -> return a

main :: IO ()
main = do
  comicFont <- getFontPath "KMKDSP__.ttf"
  (rz,win)  <- reacquire 0 $ startupSDL2Backend 800 600 "Immediate mode" True
  t         <- newTime
  let firstFrame = Frame { _frameTime   = t
                         , _frameEvents = []
                         , _frameNextK  = 0
                         , _frameWindow = win
                         , _frameRez    = rz
                         , _frameScene  = emptyScene
                         }
  void $ runStateT (runFrame $ setupFrame comicFont) firstFrame
  putStrLn "done!"
