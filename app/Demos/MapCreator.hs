{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Demos.MapCreator ( demo ) where

import SDL
import Gelatin.SDL2
import Gelatin.Fruity
import Odin.Core
import Odin.GUI
--import Odin.GUI.TextInput.Internal
import Paths_odin
import System.FilePath
import System.Exit (exitFailure)

setScaleToWindowSize :: Entity -> V2 Float -> GLuint -> Maybe (V2 Float)
                     -> System Script
setScaleToWindowSize k sz@(V2 tw th) tex mwin = do
  win <- ask
  V2 cw ch <- io (SDL.get $ windowSize win)
  let [w,h] = map fromIntegral [cw,ch]
  when ((Just $ V2 w h) /= mwin) $ do
    dealloc k
    io $ putStrLn $ unwords ["writing new background"
                            , show sz
                            , show (w,h)
                            ]
    let p = do setTextures [tex]
               setGeometry $ geometry $ add $ fan $ vertices $ do
                 to (0, 0)
                 to (V2 w 0, V2 (w/tw) 0)
                 to (V2 w h, V2 (w/tw) (h/th))
                 to (V2 0 h, V2 0      (h/th))
    _ <- k .# texPic p
    return ()
  nextScript $ setScaleToWindowSize k sz tex $ Just $ V2 w h

demo :: Font -> System ()
demo font = do
  let path = "assets" </> "images" </> "checker-bg.png"
  checkerBgPath <- io $ getDataFileName path
  (sz,tex) <- io $ loadImage checkerBgPath >>= \case
    Nothing -> exitFailure
    Just (sz,tx) -> return (realToFrac <$> sz, tx)

  k <- fresh
  k .# name "bg"
    ## tfrm mempty
    #. script [Script $ setScaleToWindowSize k sz tex Nothing]

  let str = "Please drag and drop an image to use as a tileset"
  fresh ## name "instructions"
        ## pos (V2 0 16)
        #. colorPic (coloredString font 72 16 str $ const black)

  bview <- allocColorButtonView font "Test Button" 16 buttonPainter (io . print)
  void $ freshButton 100 bview



  return ()
