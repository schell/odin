{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Odin.Engine.Tiled where

import           Data.List       (find)
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe      (catMaybes)
import           Data.Tiled
import           Gelatin.SDL2
import           System.FilePath

type ImageTextureMap   = Map Image GLuint
type TileGLRendererMap = Map Tile Renderer2

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

allocTileRenderer :: Backend GLuint e V2V2 (V2 Float) Float Raster
                  -> Tile -> Tileset -> GLuint -> IO Renderer2
allocTileRenderer b tile Tileset{..} tex = do
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
  (_,glr) <- compilePicture b $ do
    setTextures [tex]
    setGeometry $ triangles $ do
      tri (V2 0 0, V2 uvtlx uvtly) (V2 tw 0,  V2 uvbrx uvtly) (V2 tw th, V2 uvbrx uvbry)
      tri (V2 0 0, V2 uvtlx uvtly) (V2 tw th, V2 uvbrx uvbry) (V2 0 th,  V2 uvtlx uvbry)
  return glr

mapOfTiles
  :: Backend GLuint e V2V2 (V2 Float) Float Raster
  -> TiledMap
  -> IO (Either String TileGLRendererMap)
mapOfTiles be t@TiledMap{..} = do
  img2TexMap <- allocImageTextureMap t
  let tile2SetMap = assocTileWithTileset t
      findTexBySet ts =
        case M.lookup (imageOfTileset ts) img2TexMap of
              Nothing -> Left $ "Could not find texture for tileset:" ++ show ts
              Just tex -> Right (ts, tex)
      tile2ESetTexMap = findTexBySet <$> tile2SetMap
      -- Turn the Map TileSet (Either String Texture)
      -- to (Either String (Map TileSet Texture))
      f (Left err) k (Left anotherErr) = Left $ unlines [err, anotherErr]
      f (Right m)  k (Right v)         = Right $ M.insert k v m
      f (Right m)  k (Left err)        = Left err
      f (Left err) k (Right _)         = Left err
      etile2SetTexMap = M.foldlWithKey f (Right mempty) tile2ESetTexMap
  case etile2SetTexMap of
    Right m  -> do
      tm <- sequence $ M.mapWithKey (\a (b, c) -> allocTileRenderer be a b c) m
      return $ Right tm
    Left err -> return $ Left err

allocLayerRenderer
  :: TiledMap
  -> TileGLRendererMap
  -> String
  -> IO (Maybe Renderer2)
allocLayerRenderer tmap rmap name = case layerWithName tmap name of
  Nothing -> return Nothing
  Just layer -> do
    let  renderLayer :: [RenderTransform2] -> IO ()
         renderLayer tfrm = mapM_ (tileRenderer tfrm) $ M.toList $ layerData layer
         tileRenderer tfrm ((x,y), tile) = case M.lookup tile rmap of
           Nothing -> putStrLn $ "Could not find renderer for tile:" ++ show tile
           Just f  -> do
             let ts = tilesetOfTile tmap tile
                 w  = tsTileWidth ts
                 h  = tsTileHeight ts
                 v  = fromIntegral <$> V2 (w * x)  (h * y)
                 t  = tfrm ++ [Spatial $ Translate v]
             snd f t
    return $ Just (return (), renderLayer)

allocLayerFromTiledMap
  :: Backend GLuint e V2V2 (V2 Float) Float Raster
  -> TiledMap
  -> String
  -> IO (Either String Renderer2)
allocLayerFromTiledMap v2v2 tmap name = mapOfTiles v2v2 tmap >>= \case
    Left err -> return $ Left err
    Right tileRendererMap ->
      allocLayerRenderer tmap tileRendererMap name >>= \case
        Nothing -> return $ Left $ "Could not find layer " ++ show name
        Just layerRenderer -> return $ Right layerRenderer
