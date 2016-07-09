{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
import           Gelatin.Core
import           Gelatin.Picture
import           Gelatin.SDL2
import           SDL
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import qualified Data.Map.Strict as M
import           Data.Word (Word32)
import           Data.Tiled
import           Data.Monoid
import           Data.Either (lefts, rights)
import           Control.Monad (void, forever, when, forM_)
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class (liftIO)
import           Control.Varying
import           Control.Concurrent (threadDelay)
import           System.Exit (exitSuccess, exitFailure)
import           Text.Show.Pretty

import           App.Framework (isQuit)
import           Data.Tiled.Utils

data Update = UpdateTransform PictureTransform
data Input  = InputTime Float
type Script = SplineT Input [Update] IO ()
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
data Components = Components { compRndring  :: IntMap GLRenderer
                             , compTrnsfrm  :: IntMap PictureTransform
                             , compScripts  :: IntMap [Script]
                             , compName     :: IntMap String
                             }

emptyComponents :: Components
emptyComponents = Components mempty mempty mempty mempty
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
data SystemData = SystemData { sysNextUid    :: Uid
                             , sysRez        :: Rez
                             , sysLastTime   :: Word32
                             , sysComponents :: Components
                             }

emptySystemData :: Rez -> Word32 -> SystemData
emptySystemData r t = SystemData 0 r t emptyComponents

type System = StateT SystemData IO

destroyEntity :: Uid -> System ()
destroyEntity (Uid k) = do
  Components{..} <- gets sysComponents
  let mr = IM.lookup k compRndring
      mc = fst <$> mr
      r  = IM.delete k compRndring
      t  = IM.delete k compTrnsfrm
      s  = IM.delete k compScripts
      n  = IM.delete k compName
  modify' $ \sys -> sys{sysComponents = Components r t s n}
  liftIO $ sequence_ mc
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
freshUid :: System Uid
freshUid = do
  uid <- gets sysNextUid
  modify' $ \s -> s{sysNextUid = succ uid}
  return uid

tickTime :: System Float
tickTime = do
  lastT <- gets sysLastTime
  t     <- liftIO ticks
  modify' $ \s -> s{sysLastTime = t}
  return $ fromIntegral (t - lastT) / 1000

-- | Tick all component scripts by a time delta, updating them in place.
-- Returns a list of component updates and the Uids of components whose scripts
-- have ended.
tickScripts :: Float -> System ([(Uid, [Update])], [Uid])
tickScripts dt = do
  c <- gets sysComponents
  let m = compScripts c
      run s = runSplineE s $ InputTime dt
      getSteps = sequence $ mapM run <$> m
  steps <- (unzip <$>) <$> liftIO getSteps
  let uidsToEs = IM.toList $ fst <$> steps
      (uids,es) = unzip uidsToEs
      pairs = zipWith f uids $ concat es
      f k (Left u) = Left (Uid k, u)
      f k (Right ()) = Right $ Uid k
      nexts = snd <$> steps
  modify' $ \s -> s{sysComponents = c{compScripts = nexts}}
  return (lefts pairs, rights pairs)

tickUpdates :: [(Uid, [Update])] -> [Uid] -> System ()
tickUpdates us deadIds = do
  let updates uid = mapM_ (update uid)
      update uid (UpdateTransform t) = setComponentTransform uid t
  mapM_ (uncurry updates) us
  mapM_ destroyEntity deadIds

setComponentRendering :: Uid -> GLRenderer -> System ()
setComponentRendering (Uid uid) r = do
  c <- gets sysComponents
  let m = compRndring c
  modify' $ \s -> s{sysComponents = c{compRndring = IM.insert uid r m }}

setComponentTransform :: Uid -> PictureTransform -> System ()
setComponentTransform (Uid uid) p = do
  c <- gets sysComponents
  let m = compTrnsfrm c
  modify' $ \s -> s{sysComponents = c{compTrnsfrm = IM.insert uid p m }}

setComponentScripts :: Uid -> [Script] -> System ()
setComponentScripts (Uid uid) v = do
  c <- gets sysComponents
  let m = compScripts c
  modify' $ \s -> s{sysComponents = c{compScripts = IM.insert uid v m }}

addComponentScript :: Uid -> Script -> System ()
addComponentScript (Uid uid) v = do
  c <- gets sysComponents
  let m = case IM.lookup uid $ compScripts c of
            Nothing -> IM.insert uid [] m
            Just _  -> m
  modify' $ \s -> s{sysComponents = c{compScripts = IM.adjust (v:) uid m }}

setComponentName :: Uid -> String -> System ()
setComponentName (Uid uid) v = do
  c <- gets sysComponents
  let m = compName c
  modify' $ \s -> s{sysComponents = c{compName = IM.insert uid v m}}

deltaTime :: Monad m => VarT m Input Float
deltaTime = var f
  where f (InputTime t) = t
        --f _ = 0

compilePic :: Picture GLuint () -> System GLRenderer
compilePic pic = do
  rez <- gets sysRez
  fst <$> liftIO (compilePictureRenderer rez mempty pic)

makeImageComponent :: Image -> System (Maybe Uid)
makeImageComponent Image{..} = do
  e    <- freshUid
  liftIO (loadImageAsTexture iSource) >>= \case
    Nothing  -> return Nothing
    Just tex -> do
      let sz = realToFrac <$> V2 iWidth iHeight
      r <- compilePic $ withTexture tex $ rectangle 0 sz (/sz)
      e `setComponentTransform` mempty
      e `setComponentRendering` r
      return $ Just e

allocTileRendering :: Tile -> Tileset -> GLuint -> System GLRenderer
allocTileRendering tile Tileset{..} tex = do
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
  compilePic $ withTexture tex $ do
    tri (V2 0 0, V2 uvtlx uvtly) (V2 tw 0,  V2 uvbrx uvtly) (V2 tw th, V2 uvbrx uvbry)
    tri (V2 0 0, V2 uvtlx uvtly) (V2 tw th, V2 uvbrx uvbry) (V2 0 th,  V2 uvtlx uvbry)

mapOfTiles :: TiledMap -> System TileGLRendererMap
mapOfTiles t@TiledMap{..} = do
  img2TexMap <- liftIO $ allocImageTextureMap t
  let tile2SetMap = assocTileWithTileset t
      tile2SetTexMapM  = findTexBySet <$> tile2SetMap
      findTexBySet ts =
        case M.lookup (imageOfTileset ts) img2TexMap of
              Nothing -> do putStrLn $
                              "Could not find texture for tileset:" ++ show ts
                            exitFailure
              Just tex -> return (ts, tex)
  tile2SetTexMap <- liftIO $ sequence tile2SetTexMapM
  sequence $ M.mapWithKey (\a (b, c) -> allocTileRendering a b c) tile2SetTexMap

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

setupNetwork :: System ()
setupNetwork = do
  tmap@TiledMap{..} <- unrelativizeImagePaths <$>
    liftIO (loadMapFile "assets/oryx_ultimate_fantasy/uf_examples/uf_example_1.tmx")
  liftIO $ putStrLn $ ppShow tmap
  let Just floorLayer = layerWithName tmap "floor"
  liftIO $ putStrLn $ ppShow $ layerData floorLayer
  rmap <- mapOfTiles tmap
  let layerNames = [ "floor"
                   , "water edges"
                   , "shadows"
                   , "props"
                   , "walls"
                   , "heroes"
                   , "door"
                   ]
  forM_  layerNames $ \name -> do
    Just layerRenderer <- liftIO $ allocLayerRenderer tmap rmap name
    ent  <- freshUid
    ent `setComponentTransform` mempty
    ent `setComponentRendering` layerRenderer
    ent `setComponentName` (name ++ " layer")
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
processEvent :: EventPayload -> IO ()
processEvent QuitEvent = exitSuccess
processEvent (KeyboardEvent (KeyboardEventData _ _ _ k)) =
  when (isQuit k) exitSuccess
processEvent _ = return ()

renderWith :: Rez -> Window -> System ()
renderWith rez window = do
  liftIO $ clearFrame rez
  Components{..} <- gets sysComponents
  let m = IM.intersectionWith snd compRndring compTrnsfrm
  liftIO $ sequence_ m >> updateWindowSDL2 window
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
main :: IO ()
main = do
  (rez,window)  <- startupSDL2Backend 800 600 "Entity Sandbox" True
  t             <- ticks
  void $ flip runStateT (emptySystemData rez t) $ do
    setupNetwork
    liftIO $ putStrLn "Initial network created"
    forever $ do
      liftIO $ void $ pollEvents >>= mapM (processEvent . eventPayload)
      tickTime >>= tickScripts >>= uncurry tickUpdates
      renderWith rez window
      liftIO $ threadDelay 1
