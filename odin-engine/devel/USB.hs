{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
module Main where

import           Data.Function               (fix)

import           Data.List                   (find, nub, sort)
import           Data.Maybe                  (fromMaybe)

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad               (foldM, forM, forM_, msum, unless,
                                              when)
import           Control.Monad.STM           (atomically)
import qualified Data.ByteString.Char8       as B
import           Data.Tiled.Load
import           Data.Tiled.Types            (TiledMap (..))
import qualified Data.Vector                 as V
import           Devel.Utils                 (assetsDir, destroyAllocations,
                                              getWindow, iconFont,
                                              incrementRecomps, monoFont,
                                              persistAllocations)
import           Odin.Engine

import           Odin.Engine.GUI
import           Odin.Engine.Tiled
import           System.Exit                 (exitFailure)
import           System.FilePath             (FilePath, (</>))
import           System.IO
import           System.USB
import           Text.Printf
import           Text.Read                   (readMaybe)
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

-- Enumeratie all devices and find the right one.
findMyDevice :: Ctx -> VendorId -> ProductId -> IO Device
findMyDevice ctx vendorId productId = do
    devs <- V.toList <$> getDevices ctx
    deviceDescs <- mapM getDeviceDesc devs
    case fmap fst $ find (match . snd) $ zip devs deviceDescs of
      Nothing  -> hPutStrLn stderr "Device not found" >> exitFailure
      Just dev -> return dev
  where
    match :: DeviceDesc -> Bool
    match devDesc =  deviceVendorId  devDesc == vendorId
                  && deviceProductId devDesc == productId

deviceInfo :: Device -> [String]
deviceInfo dev =
  [ printf "deviceSpeed:   %s" (maybe "-" show $ deviceSpeed dev)
  , printf "busNumber:     %s" (show $ busNumber dev)
  , printf "portNumber:    %s" (show $ portNumber dev)
  , printf "portNumbers:   %s" (maybe "-" (show . V.toList) $
                                  portNumbers dev 7)
  , printf "deviceAddress: %s" (show $ deviceAddress dev)
  ]

usbIO :: IO ()
usbIO = do
  -- USB initialization:
  ctx <- newCtx
  setDebug ctx PrintDebug
  -- The teensyduino ids (which I read out of usb printout in sytsem report)
  let vendorId  = 0x16c0
      productId = 0x0483
  putStrLn "acquiring teensy"
  device    <- findMyDevice ctx vendorId productId
  withDeviceHandle device $ \devHandle -> do
    --detachKernelDriver devHandle 0
    withClaimedInterface devHandle 0 $ do
      putStrLn "Found the teensy usb serial:"
      putStrLn $ unlines $ map ("  " ++) $ deviceInfo device
      cfg <- getConfigDesc device 0
      let interface0 = V.head $ configInterfaces cfg
          alternate0 = V.head interface0
          endpoint1  = V.head $ interfaceEndpoints alternate0
          mps        = maxPacketSize $ endpointMaxPacketSize endpoint1
          timeout    = 10
      printf "maxPacketSize = %i\n" mps
      putStrLn "creating xfer..."
      xfer <- newReadTransfer
        InterruptTransfer devHandle (endpointAddress endpoint1) mps timeout
      forkIO $ fix $ \loop -> do
        (bs, status) <- performReadTransfer xfer
        print status
        putStr $ B.unpack bs
        loop
      _ <- getLine
      return ()

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
    SensorReadings n e s w d
    where go (Just (a,b)) (Just (c,d)) = Just (minimum [a,b,c,d], maximum [a,b,c,d])
          go a b = msum [a,b]
          n = go n1 n2
          e = go e1 e2
          s = go s1 s2
          w = go w1 w2
          d = go d1 d2

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
  forkIO $ fix $ \loop -> do
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
  forkIO $ fix $ \loop -> do
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


pointPicture :: Float -> V4 Float -> ColorPicture ()
pointPicture r color = setGeometry $ fan $ do
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
renderSensorPicture radius (n, e, s, w, d) point ts = do
  let units = dPadDirections
      vals  = [n,e,s,w,d]
  forM_ (zip units vals) $ \(u, v) -> do
    let t = moveV2 $ u ^* (2 * radius)
        s = scale radius radius
        nv = (fromIntegral v / 1024)
        m =  multiply nv 0 0 1
    renderPicture point $ ts ++ [t,s,m]

main :: IO ()
main = do
  -- Destroy any allocations from a previous compilation.
  runM destroyAllocations
  backends <- getWindow
  runOdinIO backends monoFont iconFont persistAllocations $ autoRelease $ do
    readingsVar <- io rawReadingsVar
    (_, pic)    <- slotColorPicture $ pointPicture 0.5 white
    saveBtn     <- slotButton buttonPainter "Save"
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
          putStrLn str
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
