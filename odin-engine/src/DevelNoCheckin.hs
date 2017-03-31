{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
module DevelMain where

import           Data.Function     (fix)

import           Control.Monad     (foldM, forM, forM_)
import           Data.Tiled.Load
import           Data.Tiled.Types  (TiledMap (..))
import qualified Data.Vector       as V
import           Devel.Utils       (assetsDir, destroyAllocations, getWindow,
                                    iconFont, incrementRecomps, monoFont,
                                    persistAllocations)
import           Odin.Engine
import           Odin.Engine.GUI
import           Odin.Engine.Tiled
import           System.FilePath   (FilePath, (</>))
import           System.USB
import           Text.Show.Pretty

tinyDungeonTmxDir :: FilePath
tinyDungeonTmxDir = assetsDir </> "oryx_tiny_dungeon" </> "td_tiled_examples"

tinyDungeonExampleMapPath :: FilePath
tinyDungeonExampleMapPath = tinyDungeonTmxDir </> "Tiny_dungeon_example.tmx"

tinyDungeonExampleAnimationMapPath :: FilePath
tinyDungeonExampleAnimationMapPath = tinyDungeonTmxDir </> "animation.tmx"

loadTinyDungeonMap :: Member IO r => FilePath -> Eff r TiledMap
loadTinyDungeonMap = fmap unrelativizeImagePaths . io . loadMapFile

enumerateDevices :: OdinFrame r => Ctx -> Eff r [Slot Text]
enumerateDevices ctx = do
  DefaultFont font <- readDefaultFontDescriptor
  devices          <- V.toList <$> io (getDevices ctx)
  descriptions     <- io $ mapM getDeviceDesc devices
  configs          <- io $ mapM (`getConfigDesc` 0) devices
  forM (zip descriptions configs) $ \(desc, config) -> do
    io $ pPrint desc
    io $ pPrint config
    slotText font black $ ppShow desc

runner :: OdinCont r => Eff r ()
runner = autoRelease $ do
  -- USB initialization:
  ctx <- io $ do
    ctx <- newCtx
    setDebug ctx PrintDebug
    return ctx

  -- Not very haskell, but very easy
  vheight <- slotVar 16
  measuredDescs <- enumerateDevices ctx >>= mapM (\description -> do
    V2 _ height <- sizeOfText description
    prevHeight  <- unslot vheight
    vheight `is` (height + prevHeight + 16)
    return (prevHeight, description))
  fix $ \loop -> do
    forM_ measuredDescs $ \(y, desc) -> renderText desc [move 0 y]
    next loop


main :: IO ()
main = do
  -- Destroy any allocations from a previous compilation.
  runM destroyAllocations
  backends <- getWindow
  runOdinIO backends monoFont iconFont persistAllocations runner
