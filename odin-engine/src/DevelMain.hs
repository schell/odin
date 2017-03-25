{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module DevelMain where

import           Data.Function     (fix)

import           Control.Monad     (forM_)
import           Data.Tiled.Load
import           Devel.Utils       (assetsDir, defaultFont, destroyAllocations,
                                    getWindow, iconFont, incrementRecomps,
                                    persistAllocations)
import           Odin.Engine
import           Odin.Engine.GUI
import           Odin.Engine.Tiled
import           System.FilePath   (FilePath, (</>))

tinyDungeonTmxPath :: FilePath
tinyDungeonTmxPath = assetsDir </> "oryx_tiny_dungeon"
                               </> "td_tiled_examples"
                               </> "Tiny_dungeon_example.tmx"

runner :: OdinCont r => Eff r ()
runner = autoRelease $ do
  DefaultFont font <- readDefaultFontDescriptor
  n                <- incrementRecomps
  recompText       <- slotText font black $ unwords ["Recompilations:", show n]
  V2 textWidth _   <- sizeOfText recompText

  text <- slotText font black "Here is some text"

  tiledMap <- unrelativizeImagePaths <$> io (loadMapFile tinyDungeonTmxPath)
  v2v2     <- v2v2Backend
  elayers  <- io $
    mapM (allocLayerFromTiledMap v2v2 tiledMap) [ "Background"
                                                , "Interface"
                                                , "Shadow"
                                                , "Objects"
                                                , "Characters"
                                                , "Smoke"
                                                ]
  case sequence elayers of
    Left err -> io $ putStrLn err
    Right layers -> fix $ \loop -> do
      V2 w h <- getWindowSize
      renderText recompText [move (w - textWidth) h]
      renderText text [move 0 16]
      forM_ layers $ \layer -> io $ snd layer [scale 2 2]
      next loop

main :: IO ()
main = do
  -- Destroy any allocations from a previous compilation.
  runM destroyAllocations
  backends <- getWindow
  runOdinIO backends defaultFont iconFont persistAllocations runner
