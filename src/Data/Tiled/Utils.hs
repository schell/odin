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
import           Data.Monoid ((<>))
import           Gelatin.GL

import Odin.Core.Utils

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


allocTileRenderer :: Rez -> Tile -> Tileset -> GLuint -> IO GLRenderer
allocTileRenderer rez tile Tileset{..} tex = do
  let img = head tsImages
      -- the image's width and height in pixels
      w   = fromIntegral $ iWidth img
      h   = fromIntegral $ iHeight img
      -- the tile's width and height in pixels
      tw  = fromIntegral tsTileWidth
      th  = fromIntegral tsTileHeight
      -- the tile's width and height in uv coords
      uvw = tw / w
      uvh = th / h
      -- index of the tile relative to the tileset's initial tile
      ndx = fromIntegral $ tileGid tile - tsInitialGid
      -- number of tiles in x
      numtx  = w / tw
      -- the tile's x and y indices
      ty  = fromIntegral (floor $ ndx / numtx :: Int)
      tx  = ndx - ty * numtx
      -- the tile's uv upper left
      V2 uvtlx uvtly = V2 (tx * uvw) (ty * uvh)
      -- the tile's uv bottom left
      V2 uvbrx uvbry = V2 uvtlx uvtly + V2 uvw uvh
  compilePic rez $ withTexture tex $ do
    tri (V2 0 0, V2 uvtlx uvtly) (V2 tw 0,  V2 uvbrx uvtly) (V2 tw th, V2 uvbrx uvbry)
    tri (V2 0 0, V2 uvtlx uvtly) (V2 tw th, V2 uvbrx uvbry) (V2 0 th,  V2 uvtlx uvbry)

mapOfTiles :: Rez -> TiledMap -> IO TileGLRendererMap
mapOfTiles rez t@TiledMap{..} = do
  img2TexMap <- allocImageTextureMap t
  let tile2SetMap = assocTileWithTileset t
      tile2ESetTexMap  = findTexBySet <$> tile2SetMap
      findTexBySet ts =
        case M.lookup (imageOfTileset ts) img2TexMap of
              Nothing -> Left $ "Could not find texture for tileset:" ++ show ts
              Just tex -> Right (ts, tex)
      checkErrorsAndClean _ (Left err) = putStrLn err >> return Nothing
      checkErrorsAndClean k (Right tstx) = return $ Just (k, tstx)
  mtile2SetTexList <- mapM (uncurry checkErrorsAndClean) $ M.toList tile2ESetTexMap
  let tile2SetTexMap = M.fromList $ catMaybes mtile2SetTexList
  sequence $ M.mapWithKey (\a (b, c) -> allocTileRenderer rez a b c) tile2SetTexMap

allocLayerRenderer :: TiledMap -> TileGLRendererMap -> String -> IO (Maybe GLRenderer)
allocLayerRenderer tmap rmap name = case layerWithName tmap name of
  Nothing -> return Nothing
  Just layer -> do
    let  renderLayer :: PictureTransform -> IO ()
         renderLayer tfrm = mapM_ (tileRenderer tfrm) $ M.toList $ layerData layer
         tileRenderer tfrm ((x,y), tile) = case M.lookup tile rmap of
           Nothing -> putStrLn $ "Could not find renderer for tile:" ++ show tile
           Just f  -> do
             let ts = tilesetOfTile tmap tile
                 w  = tsTileWidth ts
                 h  = tsTileHeight ts
                 v  = fromIntegral <$> V2 (w * x)  (h * y)
                 t  = tfrm <> PictureTransform (Transform v 1 0) 1 1
             snd f t
    return $ Just (return (), renderLayer)
