{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Odin.Engine.Tiled where

import           Control.Monad          (forM_, join, void)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.List              (find)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (catMaybes, fromMaybe, maybeToList)
import           Data.Tiled
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           Data.Word              (Word32)


import           Gelatin.SDL2           hiding (trace)
import           System.FilePath

import           Odin.Engine
import           Odin.Engine.Slots

type TextureBackend e  = Backend GLuint e V2V2 (V2 Float) Float Raster
type ImageTextureMap   = Map Image GLuint
type TileGLRendererMap = Map TileIndex Renderer2

deriving instance Ord Image

-- | Prefix the image paths with the directory of the map itself.
unrelativizeImagePaths :: TiledMap -> TiledMap
unrelativizeImagePaths m@TiledMap{..} =
  let dir  = takeDirectory mapPath
      tsets = map unrelSet mapTilesets
      unrelSet t@Tileset{..} = t{ tsImages = map unrel tsImages }
      unrel i@Image{..} = i{ iSource = dir </> iSource }
  in m{ mapTilesets = tsets }

-- | The gelatin picture for rendering one specific TileIndex.
tilePicture
  :: V2 Int
  -- ^ Dimensions of the tileset image
  -> V2 Int
  -- ^ Dimensions of the tile
  -> Int
  -- ^ The index of the tile in its parent tileset.
  -> GLuint
  -- ^ The pre-alloc'd texture
  -> TexturePicture ()
tilePicture imageSize tileSize localNdx tex = do
  let -- The image's width and height in pixels
      V2 w h = fromIntegral <$> imageSize
      ---- The tile's width and height in pixels
      V2 tw th = fromIntegral <$> tileSize
      -- The tile's width and height in uv coords
      uvw = tw / w
      uvh = th / h
      -- The number of tiles in x
      numtx  = w / tw
      -- The tile's x and y indices in the parent tileset
      ndx = fromIntegral localNdx
      ty  = fromIntegral (floor $ ndx / numtx :: Int)
      tx  = ndx - ty * numtx
      -- the tile's uv upper left
      V2 uvtlx uvtly = V2 (tx * uvw) (ty * uvh)
      -- the tile's uv bottom left
      V2 uvbrx uvbry = V2 uvtlx uvtly + V2 uvw uvh
  setTextures [tex]
  setGeometry $ triangles $ do
    tri (V2 0 0, V2 uvtlx uvtly) (V2 tw 0,  V2 uvbrx uvtly) (V2 tw th, V2 uvbrx uvbry)
    tri (V2 0 0, V2 uvtlx uvtly) (V2 tw th, V2 uvbrx uvbry) (V2 0 th,  V2 uvtlx uvbry)

layerWithName :: TiledMap -> String -> Maybe Layer
layerWithName TiledMap{..} name = find ((== name) . layerName) mapLayers

allLayerTileData :: TiledMap -> Vector (Vector (Vector (Maybe TileIndex)))
allLayerTileData tm = do
  layer <- V.fromList $ mapLayers tm
  case layerContents layer of
    LayerContentsTiles dat -> return dat
    _                      -> V.empty

animationTileIndices :: TiledMap -> Vector TileIndex
animationTileIndices tiledMap = do
  tileset <- V.fromList $ mapTilesets tiledMap
  tile    <- V.fromList $ tsTiles tileset
  frame   <- fromMaybe V.empty $
    V.fromList . animationFrames <$> tileAnimation tile
  let setId = tsInitialGid tileset
      ftid  = frameTileId frame
      fgid  = setId + fromIntegral ftid
  return TileIndex{ tileIndexGid           = fgid
                  , tileIndexIsDiagFlipped = False
                  , tileIndexIsHFlipped    = False
                  , tileIndexIsVFlipped    = False
                  }

layerTileIndices :: TiledMap -> Vector TileIndex
layerTileIndices tiledMap = do
  layerTiles <- allLayerTileData tiledMap
  row        <- layerTiles
  flip V.concatMap row $ \case
    Just ndx -> return ndx
    Nothing  -> V.empty

allTileIndices :: TiledMap -> Vector TileIndex
allTileIndices tiledMap =
  vnub $ layerTileIndices tiledMap V.++ animationTileIndices tiledMap
  -- | TODO: remove this when vector supports nub

vnub :: Eq a => Vector a -> Vector a
vnub = vnubBy (==)

vnubBy :: (a -> a -> Bool) -> Vector a -> Vector a
vnubBy eq vs
  | Just x <- vs V.!? 0
  , xs <- V.tail vs = x `V.cons` vnubBy eq (V.filter (\y -> not (eq x y)) xs)
  | otherwise = V.empty

allImages :: TiledMap -> [Image]
allImages tiledMap = do
  tileset <- mapTilesets tiledMap
  tsImages tileset

allAnimations :: TiledMap -> [(Word32, Word32, Animation)]
allAnimations tiledMap = do
  tileset <- mapTilesets tiledMap
  tile    <- tsTiles tileset
  ani     <- maybeToList $ tileAnimation tile
  return (tsInitialGid tileset, tileId tile, ani)

allocImageTexture :: Image -> IO (Maybe (Image, GLuint))
allocImageTexture i@Image{..} = ((i,) <$>) <$> loadImageAsTexture iSource

allocImageTextureMap :: TiledMap -> IO ImageTextureMap
allocImageTextureMap t = do
  mts <- mapM allocImageTexture $ allImages t
  return $ M.fromList $ catMaybes mts

imageOfTileset :: Tileset -> Maybe Image
imageOfTileset ts = case tsImages ts of
  img:_ -> Just img
  []    -> Nothing

tilesetOfTileIndex :: TiledMap -> TileIndex -> Maybe Tileset
tilesetOfTileIndex TiledMap{..} TileIndex{..} =
  case takeWhile ((<= tileIndexGid) . tsInitialGid) mapTilesets of
    [] -> Nothing
    ts -> Just $ last ts

imageOfTileIndex :: TiledMap -> TileIndex -> Maybe Image
imageOfTileIndex tm t = tilesetOfTileIndex tm t >>= imageOfTileset

imageTextureOfTile
  :: ImageTextureMap -> TiledMap -> TileIndex -> Maybe (Image, GLuint)
imageTextureOfTile imap tmap ndx = do
  img <- imageOfTileIndex tmap ndx
  tex <- M.lookup img imap
  return (img, tex)

-- | Alloc resources for one specific tile renderer.
allocTileRenderer
  :: TextureBackend e
  -- ^ The texture rendering backend.
  -> GLuint
  -- ^ The texture to use for the tileset.
  -> Tileset
  -- ^ The Tileset in which the Tile resides.
  -> TileIndex
  -- ^ The TileIndex to create a renderer for.
  -> IO Renderer2
allocTileRenderer b tex ts ndx =
  snd <$> compilePicture b (tilePicture imageSize tileSize localNdx tex)
  where imageSize = case tsImages ts of
                      img:_ -> V2 (iWidth img) (iHeight img)
                      _     -> 0
        tileSize = V2 (tsTileWidth ts) (tsTileHeight ts)
        localNdx = fromIntegral $ tileIndexGid ndx - tsInitialGid ts

newtype VectorTileRenderer = VectorTileRenderer
  { unVectorTileRenderer :: Vector (Maybe ([RenderTransform2] -> IO ()))}

createVectorTileRenderer
  :: ( MonadIO m
     , MonadSafe m
     , ReadsRenderers m
     )
  => TiledMap
  -> m VectorTileRenderer
createVectorTileRenderer tiledMap = do
  texMap   <- liftIO $ allocImageTextureMap tiledMap
  v2v2     <- v2v2Backend
  let indices    = allTileIndices tiledMap
      indexMap   = M.fromList $ V.toList $ V.zip (V.map tileIndexGid indices) indices
      vectorSize = if V.length indices == 0
                   then 0
                   else 1 + fromIntegral (tileIndexGid $ V.last indices)
  v <- V.generateM vectorSize $ \ndx -> sequence $ do
    tileNdx  <- M.lookup (fromIntegral ndx) indexMap
    (_, tex) <- imageTextureOfTile texMap tiledMap tileNdx
    tileset  <- tilesetOfTileIndex tiledMap tileNdx
    return $ do
      (clean, rend) <- liftIO $ allocTileRenderer v2v2 tex tileset tileNdx
      void $ register $ liftIO clean
      return rend
  return $ VectorTileRenderer v

tileRendererPreview
  :: MonadIO m
  => VectorTileRenderer
  -> V2 Float
  -> Int
  -> Vector ([RenderTransform2] -> m ())
tileRendererPreview (VectorTileRenderer vec) (V2 tw th) w =
  flip V.map posVec $ \((x, y), renderTile) t ->
    liftIO $ renderTile $ t ++ [move (x*tw) (y*th)]
  where posVec :: Vector ((Float, Float), [RenderTransform2] -> IO ())
        posVec = go (V.concatMap (V.fromList . maybeToList) vec) 0
        go v y = let (x, xs) = V.splitAt w v
                     h       = V.zip (row y) x
                     t       = if V.null xs then V.empty else go xs (y + 1)
                 in h V.++ t
        row :: Float -> Vector (Float, Float)
        row y  = V.generate w $ \x -> (fromIntegral x, y)

renderTilePreview
  :: MonadIO m
  => Vector ([RenderTransform2] -> m ())
  -> [RenderTransform2]
  -> m ()
renderTilePreview v t = forM_ v ($ t)

data TiledAnimation =
  TiledAnimation { taFrames        :: Vector (Int, Maybe ([RenderTransform2] -> IO ()))
                 , taFrameIndex    :: Int
                 , taFrameTimeLeft :: Int
                 }

allocTiledAnimation
  :: (MonadIO m, MonadSafe m)
  => VectorTileRenderer
  -> Word32
  -- ^ Tilset gid
  -> Animation
  -> m (Slot TiledAnimation)
allocTiledAnimation (VectorTileRenderer vec) sid (Animation frames) = do
  let mkFrame (Frame fid dur) = (dur, join $ vec V.!? fromIntegral (sid + fid))
      renderFrames = V.fromList $ map mkFrame frames
  slotVar $ TiledAnimation{ taFrames = renderFrames
                          , taFrameIndex = 0
                          , taFrameTimeLeft = 0
                          }

advanceTiledAnimation
  :: (MonadIO m, Mutate SystemTime m)
  => Slot TiledAnimation
  -> m ()
advanceTiledAnimation ani = do
  delta <- readTimeDeltaMillis
  ta@TiledAnimation{..} <- unslot ani
  let sz          = V.length taFrames
      nextNdx     = ((taFrameIndex + 1) `mod` sz) `mod` sz
      (ndx, left) = fromMaybe (taFrameIndex, 0) $
        if delta > taFrameTimeLeft
          then do
            (nextDur, _) <- taFrames V.!? nextNdx
            return (nextNdx, nextDur - (delta - taFrameTimeLeft))
          else Just (taFrameIndex, taFrameTimeLeft - delta)
          --then (taFrameIndex, taFrameTimeLeft - delta)
          --else (nextNdx, dur - (delta - taFrameTimeLeft))
  ani `is` ta{ taFrameIndex    = ndx
             , taFrameTimeLeft = left
             }

renderTiledAnimation
  :: (MonadIO m, Mutate SystemTime m)
  => Slot TiledAnimation
  -> [RenderTransform2]
  -> m ()
renderTiledAnimation ani ts = do
  TiledAnimation{..} <- unslot ani
  sequence_ $ do
    (_, mrend) <- taFrames V.!? taFrameIndex
    rend       <- mrend
    return $ liftIO $ rend ts
