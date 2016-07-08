{-# LANGUAGE RecordWildCards #-}
module Data.Tiled.Utils where

import Data.Tiled
import System.FilePath

unrelativizeImagePaths :: TiledMap -> TiledMap
unrelativizeImagePaths m@TiledMap{..} =
  let dir  = takeDirectory mapPath
      tsets = map unrelSet mapTilesets
      unrelSet t@Tileset{..} = t{ tsImages = map unrel tsImages }
      unrel i@Image{..} = i{ iSource = dir </> iSource }
  in m{ mapTilesets = tsets }

allImages :: TiledMap -> [Image]
allImages TiledMap{..} = concatMap tsImages mapTilesets
