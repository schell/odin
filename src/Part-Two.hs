module Main where

import Odin.Infrastructure
import Odin.Font
import Gelatin.GL
import Control.Varying
import System.Exit
import SDL.Event hiding (Event)

data StartScreen = StartScreenOn
                 | StartScreenOff Float

drawStartScreen :: FontData -> FontData -> StartScreen -> Pic
drawStartScreen hdr rdr StartScreenOn = do
    let fill = FillColor $ \(V2 x y) -> V4 (x/100) (y/100) 1 1 
        logo = withFont hdr $ withFill fill $ letters 128 64 "Odin"
        instructions = withFont rdr $ withFill (solid grey) $ 
                         letters 128 16 "Press any key to play" 
    move (V2 0 64) logo
    
drawStartScreen hdr rdr (StartScreenOff a) = 
    withFill (solid $ white `alpha` a) $ 
        drawStartScreen hdr rdr StartScreenOn

loadFont :: FilePath -> IO FontData
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

networkSpline :: FontData -> FontData -> SplineT UserInput Pic Effect () 
networkSpline deutsch hack = do
    let center wsize pic = move ((wsize / 2) - (pictureSize pic / 2)) pic
        drawScreen = drawStartScreen deutsch hack
        fadeScreen = drawScreen . StartScreenOff 
        winFloat = fmap fromIntegral <$> windowSize

    pure (drawScreen StartScreenOn) `untilEvent_` 
        anyKeydown >>= step

    logStr "fading out"

    (center <$> winFloat <*> (fadeScreen <$> (1 - percentageOfSeconds 1))) 
        `untilEvent_` (time ~> after 1) >>= step

main :: IO ()
main = do
    -- Load our header font
    deutsch <- loadFont "Deutsch.ttf"
    -- Load our reading font
    hack <- loadFont "Hack-Regular.ttf"
    run $ outputStream blank $ networkSpline deutsch hack 

