{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
import           Gelatin.Core
import           Gelatin.Picture
import           Gelatin.SDL2 hiding (E)
import           SDL hiding (Event, get)
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import qualified Data.Map.Strict as M
import           Data.Word (Word32)
import           Data.Tiled
import           Data.Monoid
import           Data.Maybe (mapMaybe)
import           Control.Monad (void, forever, when, unless, forM_, msum)
--import           Control.Monad.Trans.State
import           Control.Concurrent (threadDelay)
import           Control.Monad.Freer
import           Control.Monad.Freer.Internal
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.Fresh
import           System.Exit (exitSuccess, exitFailure)
import           Text.Show.Pretty hiding (Name)

import           App.Framework (isQuit)
import           Data.Tiled.Utils

data Time = Time { timeLast  :: Word32
                 , timeDelta :: Float
                 } deriving (Show, Eq)
newtype Name = Name String
type Entity = Int
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
type Reads a = Member (Reader a)
type Modifies a = Member (State a)
type ModifiesComponent a = Modifies (IntMap a)
type DoesIO = Member IO
type MakesEntities = Member Fresh
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
type Component a = State (IntMap a)

type ScriptStep = System Script

data Script = Script { unScript :: ScriptStep }
            | ScriptEnd

isRunningScript :: Script -> Bool
isRunningScript ScriptEnd = False
isRunningScript _ = True

runScript :: Script -> System Script
runScript (Script s) = s
runScript ScriptEnd = return ScriptEnd

type System = Eff '[Component Name
                   ,Component PictureTransform
                   ,Component GLRenderer
                   ,Fresh
                   ,Reader Window
                   ,Reader Rez
                   ,State [EventPayload]
                   ,State Time
                   ,State [Script]
                   ,IO
                   ]

type SystemResult a =
  ((((((a, IntMap Name), IntMap PictureTransform), IntMap GLRenderer), [EventPayload]), Time), [Script])

runSystem :: Rez -> Window -> System a -> IO (SystemResult a)
runSystem rez window system =
  runM
  $ flip runState []
  $ flip runState (Time 0 0)
  $ flip runState []
  $ flip runReader rez
  $ flip runReader window
  $ flip runFresh' 0
  $ flip runState (mempty :: IntMap GLRenderer)
  $ flip runState (mempty :: IntMap PictureTransform)
  $ runState system (mempty :: IntMap Name)
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
--destroyEntity :: Uid -> System ()
--destroyEntity (Uid k) = do
--  Components{..} <- gets sysComponents
--  let mr = IM.lookup k compRndrer
--      mc = fst <$> mr
--      r  = IM.delete k compRndrer
--      t  = IM.delete k compTrnsfrm
--      s  = IM.delete k compScripts
--      n  = IM.delete k compName
--  modify' $ \sys -> sys{sysComponents = Components r t s n}
--  io $ sequence_ mc
----------------------------------------------------------------------------------
--
----------------------------------------------------------------------------------
tickTime :: (Modifies Time r, DoesIO r) => Eff r ()
tickTime = do
  Time lastT _ <- get
  t            <- io ticks
  let dt = fromIntegral (t - lastT) / 1000
  put $ Time t dt

tickEvents :: (Modifies [EventPayload] r, DoesIO r) => Eff r ()
tickEvents = io (pollEvents >>= mapM (processEvent . eventPayload)) >>= put

tickScripts :: System ()
tickScripts = do
  -- Get the current scripts to run
  scripts <- getScripts
  -- Clear out the scripts because running the current set of scripts will
  -- possibly add new scripts
  put ([] :: [Script])
  -- Run the scripts and filter to remove any dead ones
  results <- mapM runScript scripts
  let remaining = filter isRunningScript results
  -- Add the remaining scripts on the end of any new ones
  modify (++ remaining)
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
getTimeDelta :: Modifies Time r => Eff r Float
getTimeDelta = timeDelta <$> get

setRenderer :: ModifiesComponent GLRenderer r
            => Entity -> GLRenderer -> Eff r ()
setRenderer k r = modify (IM.insert k r)

setTransform :: ModifiesComponent PictureTransform r
             => Entity -> PictureTransform -> Eff r ()
setTransform k p = modify (IM.insert k p)

getTransform :: ModifiesComponent PictureTransform r
             => Entity -> Eff r (Maybe PictureTransform)
getTransform k = IM.lookup k <$> get

modifyTransform :: ModifiesComponent PictureTransform r
                => Entity -> (PictureTransform -> PictureTransform) -> Eff r ()
modifyTransform k f =
  getTransform k >>= \case
    Nothing -> setTransform k $ f mempty
    Just t  -> setTransform k $ f t

getScripts :: Modifies [Script] r => Eff r [Script]
getScripts = get

addScript :: Modifies [Script] r => ScriptStep -> Eff r ()
addScript = modify . (:) . Script

addScripts :: (Modifies [Script] r) => [Script] -> Eff r ()
addScripts = modify . (++)

setName :: ModifiesComponent Name r => Entity -> Name -> Eff r ()
setName k n = modify $ IM.insert k n

getEvents :: Modifies [EventPayload] r => Eff r [EventPayload]
getEvents = get
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
compilePic :: (Reads Rez r, DoesIO r)
           => Picture GLuint () -> Eff r GLRenderer
compilePic pic = do
  rez <- ask
  fst <$> io (compilePictureRenderer rez mempty pic)

allocTileRenderer :: (Reads Rez r, DoesIO r)
                  => Tile -> Tileset -> GLuint -> Eff r GLRenderer
allocTileRenderer tile Tileset{..} tex = do
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

mapOfTiles :: (Reads Rez r, DoesIO r) => TiledMap -> Eff r TileGLRendererMap
mapOfTiles t@TiledMap{..} = do
  img2TexMap <- io $ allocImageTextureMap t
  let tile2SetMap = assocTileWithTileset t
      tile2SetTexMapM  = findTexBySet <$> tile2SetMap
      findTexBySet ts =
        case M.lookup (imageOfTileset ts) img2TexMap of
              Nothing -> do putStrLn $
                              "Could not find texture for tileset:" ++ show ts
                            exitFailure
              Just tex -> return (ts, tex)
  tile2SetTexMap <- io $ sequence tile2SetTexMapM
  sequence $ M.mapWithKey (\a (b, c) -> allocTileRenderer a b c) tile2SetTexMap

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
----------------------------------------------------------------------------------
---- Scripts
----------------------------------------------------------------------------------
--waitUntil :: ScriptEvent c -> Script c
--waitUntil = _untilEvent (pure ())

data Direction = North | East | South | West deriving (Show, Eq, Bounded)

codeToDirection :: Scancode -> Maybe Direction
codeToDirection ScancodeUp = Just North
codeToDirection ScancodeRight = Just East
codeToDirection ScancodeDown = Just South
codeToDirection ScancodeLeft = Just West
codeToDirection _ = Nothing

directionToCode :: Direction -> Scancode
directionToCode North = ScancodeUp
directionToCode East  = ScancodeRight
directionToCode South = ScancodeDown
directionToCode West  = ScancodeLeft

directionToV2 :: Direction -> V2 Float
directionToV2 North = V2 0 (-1)
directionToV2 East = V2 1 0
directionToV2 South = V2 0 1
directionToV2 West = V2 (-1) 0

arrowCodes :: [Scancode]
arrowCodes = [ScancodeUp, ScancodeLeft, ScancodeDown, ScancodeRight]

onTrue :: (a -> Bool) -> a -> Maybe a
onTrue f x = if f x then Just x else Nothing

arrowControl :: (Modifies [EventPayload] r
                ,Modifies [Script] r
                ,Modifies Time r
                ,ModifiesComponent PictureTransform r
                ) => Entity -> Eff r Script
arrowControl actor = do
  -- First wait until the user presses an arrow key
      -- For that we'll need some scafolding so we can test and extract the
      -- arrow direction
  let isArrowPressed (KeyboardEvent (KeyboardEventData _ Pressed False Keysym{..})) =
        msum $ map (onTrue (== keysymScancode)) arrowCodes
      isArrowPressed _ = Nothing
  events <- getEvents
  let codes = mapMaybe isArrowPressed events
  unless (null codes) $ do
    let dirs = mapMaybe codeToDirection codes
    -- If we got a direction then apply the arrow move script to each
    -- direction.
    unless (null dirs) $ do
      scripts <- mapM (arrowControlMove actor) dirs
      addScripts scripts
  return $ Script $ arrowControl actor

arrowControlMove :: (Modifies [EventPayload] r
                    ,Modifies Time r
                    ,ModifiesComponent PictureTransform r
                    ) => Entity -> Direction -> Eff r Script
arrowControlMove actor dir = do
  -- Update the transform of the actor
  dt <- getTimeDelta
  let tfrm = PictureTransform (Transform (dt * 100 *^ directionToV2 dir) 1 0) 1 1
  modifyTransform actor (tfrm <>)
  -- Find if the arrow key was released
  let isArrowReleased (KeyboardEvent (KeyboardEventData _ Released False Keysym{..})) =
        keysymScancode == directionToCode dir
      isArrowReleased _ = False
  released <- any isArrowReleased <$> getEvents
  --  Restart the process all over again, from the top
  if released
    then return ScriptEnd
    else return $ Script $ arrowControlMove actor dir
--
setupNetwork :: System ()
setupNetwork = do
  tmap@TiledMap{..} <- unrelativizeImagePaths <$>
    io (loadMapFile "assets/oryx_ultimate_fantasy/uf_examples/uf_example_1.tmx")
  io $ putStrLn $ ppShow tmap
  let Just floorLayer = layerWithName tmap "floor"
  io $ putStrLn $ ppShow $ layerData floorLayer
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
    Just layerRenderer <- io $ allocLayerRenderer tmap rmap name
    ent  <- fresh
    ent `setTransform` mempty
    ent `setRenderer` layerRenderer
    ent `setName` (Name $ name ++ " layer")

  hero <- fresh
  heroRnd <- compilePic (withColor $ rectangle 0 20 $ const red)
  hero `setRenderer` heroRnd
  hero `setTransform` mempty
  addScript $ arrowControl hero
----------------------------------------------------------------------------------
----
----------------------------------------------------------------------------------
processEvent :: EventPayload -> IO EventPayload
processEvent QuitEvent = exitSuccess
processEvent ev@(KeyboardEvent key@(KeyboardEventData _ m r k)) = do
  when (isQuit k) exitSuccess
  --print key
  return ev
processEvent e = return e

getRenderers :: Member (State (IntMap GLRenderer)) r
             => Eff r (IntMap GLRenderer)
getRenderers = get

getTransforms :: Member (State (IntMap PictureTransform)) r
              => Eff r (IntMap PictureTransform)
getTransforms = get

renderWith :: ( Member (State (IntMap GLRenderer)) r
              , Member (State (IntMap PictureTransform)) r
              , Member IO r)
           => Rez -> Window -> Eff r ()
renderWith rez window = do
  io $ clearFrame rez
  renderers  <- getRenderers
  transforms <- getTransforms
  let m = IM.intersectionWith snd renderers transforms
  io $ sequence_ m >> updateWindowSDL2 window
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
io :: Member IO r => IO a -> Eff r a
io = send

main :: IO ()
main = do
  (rez,window)  <- startupSDL2Backend 800 600 "Entity Sandbox" True
  void $ runSystem rez window $ do
    setupNetwork
    forever $ do
      tickTime
      tickEvents
      tickScripts
      renderWith rez window
      io $ threadDelay 1
