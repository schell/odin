module Main where

import Odin.Infrastructure
import Odin.Font
import Gelatin.GL
import Control.Varying
import Control.Monad.IO.Class
import System.Exit
import SDL.Event hiding (Event)

data StartScreen = StartScreenOn
                 | StartScreenOff Float

drawStartScreen :: Font -> Font -> StartScreen -> Pic
drawStartScreen hdr rdr StartScreenOn = move (V2 0 64) $ do
    withFont hdr $ withFill (FillColor white) $ letters 64 "Odin"
    move (V2 0 16) $ 
        withFont rdr $ withFill (FillColor grey) $ 
            letters 16 "Press any key to play" 
drawStartScreen hdr rdr (StartScreenOff a) = 
    withFill (FillColor $ white `alpha` a) $ 
        drawStartScreen hdr rdr StartScreenOn

loadFont :: FilePath -> IO Font
loadFont fp = do
    efont <- odinFont fp 
    case efont of 
        Left err -> do putStrLn err 
                       exitFailure 
        Right f  -> return f

anyKeyup :: Monad m => VarT m UserInput (Event ())
anyKeyup = var f ~> onJust
    where f (InputKey Released _ _) = Just ()
          f _ = Nothing

anyKeydown :: Monad m => VarT m UserInput (Event ())
anyKeydown = var f ~> onJust
    where f (InputKey Pressed _ _) = Just ()
          f _ = Nothing

percentageOfSeconds :: Float -> VarT Effect UserInput Float
percentageOfSeconds s = (/ s) <$> (time ~> accumulate (+) 0)

networkSpline :: Font -> Font -> SplineT UserInput Pic Effect () 
networkSpline deutsch hack = do
    let drawScreen = drawStartScreen deutsch hack
        fadeScreen = drawScreen . StartScreenOff 

    pure (drawScreen StartScreenOn) `untilEvent_` anyKeydown >>= step
    liftIO $ putStrLn "fading"
    (fadeScreen <$> (1 - percentageOfSeconds 1 ~> vtrace)) `untilEvent_` anyKeydown >>= step
    liftIO $ putStrLn "looping"
    networkSpline deutsch hack

main :: IO ()
main = do
    -- Load our header font
    deutsch <- loadFont "Deutsch.ttf"
    -- Load our reading font
    hack <- loadFont "Hack-Regular.ttf"
    run $ outputStream blank $ networkSpline deutsch hack 

