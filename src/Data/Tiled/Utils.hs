{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Tiled.Utils where

import           Data.Tiled
import           System.FilePath
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.List (find)
import           Data.Maybe (catMaybes)
import           Gelatin.GL

type ImageTextureMap = Map Image GLuint
type TileGLRendererMap = Map Tile GLRenderer

deriving instance Ord Image

unrelativizeImagePaths :: TiledMap -> TiledMap
unrelativizeImagePaths m@TiledMap{..} =
  let dir  = takeDirectory mapPath
      tsets = map unrelSet mapTilesets
      unrelSet t@Tileset{..} = t{ tsImages = map unrel tsImages }
      unrel i@Image{..} = i{ iSource = dir </> iSource }
  in m{ mapTilesets = tsets }

allImages :: TiledMap -> [Image]
allImages TiledMap{..} = concatMap tsImages mapTilesets

layerWithName :: TiledMap -> String -> Maybe Layer
layerWithName TiledMap{..} name = find ((== name) . layerName) mapLayers

allTiles :: TiledMap -> [Tile]
allTiles = concatMap allLayerTiles . mapLayers
  where allLayerTiles = M.elems . layerData

allocImageTexture :: Image -> IO (Maybe (Image, GLuint))
allocImageTexture i@Image{..} = ((i,) <$>) <$> loadImageAsTexture iSource

allocImageTextureMap :: TiledMap -> IO ImageTextureMap
allocImageTextureMap t = do
  mts <- mapM allocImageTexture $ allImages t
  return $ M.fromList $ catMaybes mts

tilesetOfTile :: TiledMap -> Tile -> Tileset
tilesetOfTile TiledMap{..} Tile{..} =
  snd $ last $ takeWhile ((<= tileGid) . fst) initGidToTileset
    where initGidToTileset = map f mapTilesets
          f t@Tileset{..} = (tsInitialGid, t)

imageOfTileset :: Tileset -> Image
imageOfTileset = head . tsImages

imageOfTile :: TiledMap -> Tile -> Image
imageOfTile tm t = imageOfTileset $ tilesetOfTile tm t

assocTileWithTileset :: TiledMap -> Map Tile Tileset
assocTileWithTileset t@TiledMap{..} = M.fromList $ zip tiles tilesets
  where tilesets = map (tilesetOfTile t) tiles
        tiles = allTiles t


