{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
import Gelatin.SDL2
import Control.Monad (void, forever, forM_)
import Control.Concurrent (threadDelay)
import Data.Monoid ((<>))
import Text.Show.Pretty
import System.FilePath
import System.Directory
import System.Exit

import Data.Tiled
import Data.Tiled.Utils
import Odin.Common
import Odin.Component
import Odin.System
import Odin.Styles
import Odin.Scripts.ArrowControl
import Odin.Scripts.Button

-- | Load our standard fonts.
getFont :: String -> IO FontData
getFont name = do
    -- Get our fonts
    assets <- (</> "assets") <$> getCurrentDirectory
    -- Load our header font
    let font = assets </> "fonts" </> name
    loadFont font >>= \case
      Left err -> do print err
                     exitFailure
      Right fnt -> return fnt

setupNetwork :: System ()
setupNetwork = do
  -- An example of a subsystem:
  --   scripts of the outer system can only interact with the renderer, transform
  --   or name of the subsystem (its components in the outer system) and cannot
  --   access any of its inner workings.
  layers <- freshSystem $ do
    tmap@TiledMap{..} <- unrelativizeImagePaths <$>
      io (loadMapFile "assets/oryx_ultimate_fantasy/uf_examples/uf_example_1.tmx")
    io $ putStrLn $ ppShow tmap
    let Just floorLayer = layerWithName tmap "floor"
    io $ putStrLn $ ppShow $ layerData floorLayer

    rez  <- ask
    rmap <- io $ mapOfTiles rez tmap
    let layerNames = [ "floor"
                     , "water edges"
                     , "shadows"
                     , "props"
                     , "walls"
                     , "heroes"
                     , "door"
                     ]
    forM_  layerNames $ \name -> do
      Just layerRenderer <- io $ allocLayerRenderer tmap rmap name
      ent  <- fresh
      ent `setPicTransform` mempty
      ent `setRenderer` snd layerRenderer
      ent `setDealloc` fst layerRenderer
      ent `setName` (Name $ name ++ " layer")

  let tfrm = PictureTransform (Transform 10 1 0) 1 1
  layers `modifyPicTransform` (tfrm<>)

  hero <- fresh
  hero `setPicRenderer` withColor (rectangle 0 20 $ const red) >>= setDealloc hero

  hero `setPicTransform` mempty
  addScript $ arrowControl hero

  font <- io $ getFont "KMKDSP__.ttf"

  let button = ButtonData font "Testable Button" 16 buttonPainter
  void $ freshButton button 100 $ do
    io $ putStrLn "button hit!"
    endScript

main :: IO ()
main = do
  (rez,window) <- startupSDL2Backend 800 600 "Entity Sandbox" True
  putStrLn "sdl init'd"
  let step = emptySystemStep rez windw
  void $ runSystem step $ do
    setupNetwork
    io $ putStrLn "setup network"
    forever $ do
      tickSystem
      io $ threadDelay 1
