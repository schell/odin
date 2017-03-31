{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
module Main where

import           Control.Applicative ((<|>))
import           Control.Monad       (forM, forM_, msum)
import           Data.Function       (fix)
import qualified Data.Map            as M
import           Data.Tiled.Load
import           Data.Tiled.Types
import qualified Data.Vector         as V
import           Text.Show.Pretty

import           Devel.Utils
import           Odin.Engine
import           Odin.Engine.GUI

import           Odin.Engine.Tiled
import           System.FilePath     (FilePath, (</>))


tinyDungeonTmxDir :: FilePath
tinyDungeonTmxDir = assetsDir </> "oryx_tiny_dungeon" </> "td_tiled_examples"

tinyDungeonExampleMapPath :: FilePath
tinyDungeonExampleMapPath = tinyDungeonTmxDir </> "Tiny_dungeon_example.tmx"

tinyDungeonExampleAnimationMapPath :: FilePath
tinyDungeonExampleAnimationMapPath = tinyDungeonTmxDir </> "animation.tmx"

loadTinyDungeonMap :: Member IO r => FilePath -> Eff r TiledMap
loadTinyDungeonMap = fmap unrelativizeImagePaths . io . loadMapFile

runner :: OdinCont r => Eff r ()
runner = autoRelease $ do
  DefaultFont font <- readDefaultFontDescriptor
  n                <- incrementRecomps
  recompText       <- slotText font black $ unwords ["Recompilations:", show n]
  V2 textWidth _   <- sizeOfText recompText
  tiledMap         <- loadTinyDungeonMap tinyDungeonExampleMapPath
  tileRenderer     <- createVectorTileRenderer tiledMap
  let preview = V.force $ tileRendererPreview tileRenderer (V2 16 16) 64
      VectorTileRenderer vtr = tileRenderer
  aniList <- forM (allAnimations tiledMap) $ \(sid, tid, ani) -> do
    tani <- allocTiledAnimation tileRenderer sid ani
    return (fromIntegral $ sid + tid, tani)
  let animations = M.fromList aniList
  fix $ \loop -> do
    renderTilePreview preview []
    forM_ (allLayerData tiledMap) $ \ldata ->
      flip V.imapM_ ldata $ \y row ->
        flip V.imapM_ row $ \x mtindex -> sequence $ do
          let ts = [moveV2 $ 50 + (16 * (fromIntegral <$> V2 x y))]
          tindex <- mtindex
          let gid = tileIndexGid tindex
          mrend  <- vtr V.!? fromIntegral gid
          (do animation <- M.lookup (fromIntegral gid) animations
              return $ renderTiledAnimation animation ts
           <|>
           do rend   <- mrend
              return (io $ rend ts))
    V2 w h <- getWindowSize
    renderText recompText [move (w - textWidth) h]
    forM_ (zip [0 ..] $ M.elems animations) $ \(i, animation) -> do
      renderTiledAnimation animation [move (fromIntegral i * 16) 600]
      advanceTiledAnimation animation
    next loop

main :: IO ()
main = do
  -- Destroy any allocations from a previous compilation.
  runM destroyAllocations
  backends <- getWindow
  runOdinIO backends defaultFont iconFont persistAllocations runner
