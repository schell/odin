{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad               (forM, forM_, msum, void, when)
import           Control.Monad.STM           (atomically)
import           Data.Function               (fix)
import           Data.Maybe                  (fromMaybe)
import           Data.Tiled.Load
import           Data.Tiled.Types            (TiledMap (..))

import           Devel.Utils                 (assetsDir, destroyAllocations,
                                              getWindow, iconFont, monoFont,
                                              persistAllocations)
import           Odin.Engine

import           Odin.Engine.GUI
import           Odin.Engine.Tiled
import           System.FilePath             (FilePath, (</>))
import           System.IO
import           Text.Read                   (readMaybe)
--import           Text.Show.Pretty

tinyDungeonTmxDir :: FilePath
tinyDungeonTmxDir = assetsDir </> "oryx_tiny_dungeon" </> "td_tiled_examples"

tinyDungeonExampleMapPath :: FilePath
tinyDungeonExampleMapPath = tinyDungeonTmxDir </> "Tiny_dungeon_example.tmx"

tinyDungeonExampleAnimationMapPath :: FilePath
tinyDungeonExampleAnimationMapPath = tinyDungeonTmxDir </> "animation.tmx"

loadTinyDungeonMap :: Member IO r => FilePath -> Eff r TiledMap
loadTinyDungeonMap = fmap unrelativizeImagePaths . io . loadMapFile


data SensorReadings = SensorReadings { srNorth :: Maybe (Int, Int)
                                     , srEast  :: Maybe (Int, Int)
                                     , srSouth :: Maybe (Int, Int)
                                     , srWest  :: Maybe (Int, Int)
                                     , srDown  :: Maybe (Int, Int)
                                     } deriving (Show, Eq)

instance Monoid SensorReadings where
  mempty = SensorReadings e e e e e
    where e = Nothing
  mappend (SensorReadings n1 e1 s1 w1 d1) (SensorReadings n2 e2 s2 w2 d2) =
    SensorReadings n e s w down
    where go (Just (a,b)) (Just (c,d)) = Just (minimum [a,b,c,d], maximum [a,b,c,d])
          go a b = msum [a,b]
          n = go n1 n2
          e = go e1 e2
          s = go s1 s2
          w = go w1 w2
          down = go d1 d2

readSensorReadings :: String -> Maybe SensorReadings
readSensorReadings str = do
  (n, str2) <- readInt str
  (e, str3) <- readInt str2
  (s, str4) <- readInt str3
  (w, str5) <- readInt str4
  d         <- readMaybe str5
  let mk a = Just (a, a)
  return $ SensorReadings (mk n) (mk e) (mk s) (mk w) (mk d)
  where readInt s =
          (,drop 1 $ dropWhile (/= ',') s) <$> (readMaybe $ takeWhile (/=',') s)

-- | TODO: Provide a way to clean up the handle, etc
sensorReadingsVar :: IO (TVar SensorReadings)
sensorReadingsVar = do
  var    <- newTVarIO mempty
  handle <- openFile "/dev/cu.usbmodem2429121" ReadMode
  hSetBuffering handle LineBuffering
  void $ forkIO $ fix $ \loop -> do
    ln <- hGetLine handle
    case readSensorReadings ln of
      Nothing -> return ()
      Just r  -> do
        r1 <- readTVarIO var
        atomically $ writeTVar var $ mappend r1 r
    loop
  return var

-- | TODO: Provide a way to clean up the handle, etc
rawReadingsVar :: IO (TVar (Int, Int, Int, Int, Int))
rawReadingsVar = do
  var    <- newTVarIO (0, 0, 0, 0, 0)
  handle <- openFile "/dev/cu.usbmodem2429121" ReadMode
  hSetBuffering handle LineBuffering
  void $ forkIO $ fix $ \loop -> do
    ln <- hGetLine handle
    case readSensorReadings ln of
      Nothing -> return ()
      Just (SensorReadings n e s w d)  -> do
        let f = fromMaybe 0 . (fst <$>)
            nv = f n
            ev = f e
            sv = f s
            wv = f w
            dv = f d
        atomically $ writeTVar var $ (nv, ev, sv, wv, dv)

    loop
  return var


pntPicture :: Float -> V4 Float -> ColorPicture ()
pntPicture r color = setGeometry $ fan $ do
  to (0, color)
  to (V2 (-r) (-r), color)
  to (V2 r (-r), color)
  to (V2 r r, color)
  to (V2 (-r) r, color)
  to (V2 (-r) (-r), color)

dPadDirections :: [V2 Float]
dPadDirections = [V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0, 0]

renderSensorPicture
  :: OdinCont r => Float -> (Int, Int, Int, Int, Int) -> Slot Renderer2 -> [RenderTransform2] -> Eff r ()
renderSensorPicture radius (n, e, s, w, d) pnt ts = do
  let units = dPadDirections
      vals  = [n,e,s,w,d]
  forM_ (zip units vals) $ \(u, v) -> do
    let t = moveV2 $ u ^* (2 * radius)
        sc = scale radius radius
        nv = (fromIntegral v / 1024)
        m =  multiply nv 0 0 1
    renderPicture pnt $ ts ++ [t,sc,m]

main :: IO ()
main = do
  -- Destroy any allocations from a previous compilation.
  runM destroyAllocations
  backends <- getWindow
  runOdinIO backends monoFont iconFont persistAllocations $ autoRelease $ do
    readingsVar <- io rawReadingsVar
    (_, pic)    <- slotColorPicture $ pntPicture 0.5 white
    --saveBtn     <- slotButton buttonPainter "Save"
    northBtn    <- slotButton buttonPainter "North "
    eastBtn     <- slotButton buttonPainter "East  "
    southBtn    <- slotButton buttonPainter "South "
    westBtn     <- slotButton buttonPainter "West  "
    downBtn     <- slotButton buttonPainter "Down  "

    ($ (0,0,0,0,0)) $
          fix $ \loop r0@(n0, e0, s0, w0, d0) -> do
      r@(n, e, s, w, d) <- io $ readTVarIO readingsVar
      renderSensorPicture 10 r pic [move 100 100]
      let dirs = zip3 ["north", "east", "south", "west", "down"] dPadDirections
                      [northBtn, eastBtn, southBtn, westBtn, downBtn]
      maybesOfSaveDir <- forM dirs $ \(str, dv, btn) -> do
        vsize <- sizeOfButton btn
        mClicked <- partial (== ButtonStateClicked) <$>
          renderButton btn [moveV2 $ vsize * dv]
        return $ str <$ mClicked
      case msum maybesOfSaveDir of
        Just str -> do
          io $ putStrLn str
          forM_ (zip3 "neswd" [n0, e0, s0, w0, d0] [n, e, s, w, d])
                  $ \(dir, prev, val) -> when (abs (val - prev) > 1) $ io $ do
                    let v  = val - prev
                        av = abs v
                        sn = if v < 0 then "-" else "+"
                    putStrLn $ map (\c -> if c == ' ' then '\t' else c) $
                      unwords [ dir : ""
                              , sn ++ show av
                              , "|"
                              , show prev
                              , "->"
                              , show val
                              ]
          next $ loop r
        _ -> next $ loop r0
