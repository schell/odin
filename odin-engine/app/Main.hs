{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

--import           Control.Monad     (void)
import           Data.Char.FontAwesome
import           Data.Function          (fix)
import           Odin.Engine
import           Odin.Engine.Checkpoint
import           Odin.Engine.GUI
import           System.Exit            (exitFailure)
--import           Odin.Engine.Slots

simpleButtons :: OdinCont r => Eff r ()
simpleButtons = autoRelease $ do
  io $ putStrLn "Inside simpleButtons"
  DefaultFont font <- readDefaultFontDescriptor
  title <- slotText font black "Click the button"
  btn   <- slotButton buttonPainter "Okay"
  close <- slotButton iconButtonPainter [faTimes]
  fix $ \loop -> renderButton close [] >>= \case
    ButtonStateClicked -> return ()
    _ -> do
      renderText title [move 0 32]
      renderButton btn [move 0 36] >>= \case
        ButtonStateClicked -> return ()
        _ -> next loop

restart :: OdinCont r => Eff r a -> Eff r ()
restart eff = withCheckpoint "RestartCount" (pure (0 :: Int)) $ \n -> do
  -- Checkpoints are persistent through recompilations (with halive).
  updateCheckpoint "RestartCount" $ n + 1
  DefaultFont font <- readDefaultFontDescriptor
  autoRelease $ do
    note <- slotText font black $ unwords [ "The program has been (re)started"
                                          , show n
                                          , "times. \nLeft click to continue..."
                                          ]
    fix $ \loop -> do
      renderText note [move 0 16]
      queryMouseButtonEvent ButtonLeft Released 1 >>= \case
        True  -> return ()
        False -> next loop
  _ <- next eff
  autoRelease $ do
    note <- slotText font black $ unwords [ "Waiting for another file event. ("
                                          ,show n
                                          ,")"
                                          ]
    fix $ \loop -> renderText note [move 0 16] >> next loop

getWindow :: IO SDL2Backends
getWindow = runM $ withCheckpoint "window" startWindow $ \case
  Left err       -> io (putStrLn err >> exitFailure)
  Right backends -> return backends
  where startWindow = io $ startupWindow (V2 800 600) "odin-engine-exe"

defaultFont :: DefaultFont
defaultFont = DefaultFont $ fontDescriptor "../assets/fonts/KMKDSP__.ttf" 16

iconFont :: IconFont
iconFont = IconFont $ fontDescriptor "../assets/fonts/FontAwesome.otf" 16

main :: IO ()
main = do
  backends <- getWindow
  runOdinIO backends defaultFont iconFont (pure ()) $ restart $ do
    checkpoint "simpleButtons" simpleButtons
    io $ putStrLn "passed simpleButtons"
