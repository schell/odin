{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Gelatin.SDL2 hiding (get, Event, time, windowSize)
import           Codec.Picture
import           Codec.Picture.Types
import           Control.Varying
import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.RWS.Strict
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Concurrent.Async
import qualified Data.IntMap.Strict as IM
import           Data.IntMap (IntMap)
import           Data.Time.Clock
import           Data.Either (either)
import           Data.Foldable (foldl')
import           System.Exit (exitSuccess)
import           Foreign.C.String (peekCString)

import           Odin.Common

newtype Uid = Uid { unUid :: Int } deriving (Show, Eq, Ord, Enum, Num)

newtype Parent = Parent Uid

data LogLevel = LogLevelInfo
              | LogLevelWarn
              | LogLevelError
              deriving (Show, Eq, Ord, Bounded, Enum)

data Action = ActionNone
            | ActionLog LogLevel String
            | ActionGetImageSize Uid FilePath

applyAction :: Action -> IO ()
applyAction ActionNone = return ()
applyAction (ActionLog lvl str) = putStrLn $ concat [show lvl, ": ", str]

data AppData = AppData
  { appNextId :: Uid
  , appLogic  :: VarT Effect AppEvent (Picture GLuint ())
  , appCache  :: Cache IO PictureTransform
  , appUTC    :: UTCTime
  , appConfig :: OdinConfig
  , appRez    :: Rez
  }

appData :: OdinConfig -> Rez -> UTCTime -> AppData
appData cfg rez utc =
  AppData { appNextId = 0
          , appLogic  = pure blank
          , appUTC    = utc
          , appCache  = mempty
          , appConfig = cfg
          , appRez    = rez
          }

data ReadData = ReadData { rdWindowSize :: V2 Int
                         , rdOdinConfig :: OdinConfig
                         }

type StateData = Uid

type Effect = RWST ReadData [Action] StateData IO


data AppEvent = AppEventNone
              | AppEventTime Float
              | AppEventFrame
              | AppKeyEvent { keyMotion :: InputMotion
                            , keyRepeat :: Bool
                            , keySym    :: Keysym
                            }
              | AppDropEvent FilePath
              | AppQuit

data Delta = DeltaTime Float
           | DeltaFrames Int

type AppSequence = SplineT AppEvent (Picture GLuint ()) Effect
type AppSignal = VarT Effect AppEvent

handleEvent :: EventPayload -> IO AppEvent
handleEvent (KeyboardEvent (KeyboardEventData _ m r k)) =
  if isQuit k then exitSuccess else return $ AppKeyEvent m r k
handleEvent (DropEvent (DropEventData cstr)) =
  AppDropEvent <$> peekCString cstr
handleEvent _ = return AppEventNone

fresh :: Effect Uid
fresh = do
  uid <- get
  modify (+1)
  return uid

infoStr :: String -> AppSequence ()
infoStr str = lift $ tell [ActionLog LogLevelInfo str]

getIconFont :: Effect FontData
getIconFont = asks (ocIconFont . rdOdinConfig)

getFancyFont :: Effect FontData
getFancyFont = asks (ocFancyFont . rdOdinConfig)

getLegibleFont :: Effect FontData
getLegibleFont = asks (ocLegibleFont . rdOdinConfig)

asyncEvent :: MonadIO m => IO b -> VarT m a (Event (Either String b))
asyncEvent f = flip resultStream () $ do
  a <- liftIO $ async f
  let ev = varM (const $ liftIO $ poll a) ~> onJust
  meb <- pure () `_untilEvent` ev
  return $ case meb of
    Left exc -> Left $ show exc
    Right b -> Right b

reqImage :: FilePath -> AppSignal (Event (Either String (V2 Int, GLuint)))
reqImage fp = flip resultStream () $ do
  eimg <- pure () `_untilEvent` asyncEvent (readImage fp)
  case join eimg of
    Left err  -> return $ Left err
    Right img -> Right <$> liftIO (loadTexture img)

dropEvent :: Monad m => VarT m AppEvent (Event FilePath)
dropEvent = var f ~> onJust
  where f (AppDropEvent fp) = Just fp
        f _ = Nothing

anyKeydownEvent :: Monad m => VarT m AppEvent (Event ())
anyKeydownEvent = var f ~> onTrue
  where f (AppKeyEvent Pressed _ _) = True
        f _ = False

windowSize :: VarT Effect a (V2 Int)
windowSize = varM $ const $ asks rdWindowSize

halfWindowSize :: VarT Effect a (V2 Float)
halfWindowSize = (fmap fromIntegral <$> windowSize) / 2

time :: Monad m => VarT m AppEvent Float
time = var f ~> onJust ~> foldStream (+) 0
  where f (AppEventTime dt) = Just dt
        f _ = Nothing

frames :: Monad m => VarT m AppEvent Int
frames = var f ~> accumulate (+) 0 ~> vstrace "frames: "
  where f AppEventFrame = 1
        f _ = 0

faPlusSquareO :: Char
faPlusSquareO = '\xf196'

faCircleONotch :: Char
faCircleONotch = '\xf1ce'

crosshair :: V2 Float -> Picture GLuint ()
crosshair vec = move vec $ draw $ polylines [StrokeWidth 3, StrokeFeather 1] $
  do let len = 50
         c   = white `alpha` 0.75
     lineStart (0,c) $ lineTo (V2 len    0, c)
     lineStart (0,c) $ lineTo (V2 (-len) 0, c)
     lineStart (0,c) $ lineTo (V2 0    len, c)
     lineStart (0,c) $ lineTo (V2 0 (-len), c)

spinner :: Float -> V2 Float -> Picture GLuint ()
spinner t = rotate (pi * t) . crosshair

rootLogic :: AppSequence ()
rootLogic = do
  fancy <- lift getFancyFont
  icons <- lift getIconFont

  let text = "Drag and drop an image to start a new tileset."
      instructions = move (V2 0 32) $ draw $ letters $
                       filled (Name 0) fancy 128 32 text $ solid white
  fp <- pure instructions `_untilEvent` dropEvent
  infoStr $ "Dropped file " ++ fp
  markupImage fp
  rootLogic

markupImage :: FilePath -> AppSequence ()
markupImage fp = do
  eimg <- (spinner <$> time <*> halfWindowSize) `_untilEvent` reqImage fp
  infoStr $ "Got file " ++ fp
  legible <- lift $ asks (ocLegibleFont . rdOdinConfig)
  case eimg of
    Left err -> do
      let errText = do let errStr = show err
                           goStr  = "Press any key to continue."
                       move 16 $ draw $ letters $
                         filled (Name 0) legible 128 16 errStr $ solid red
                       move 32 $ draw $ letters $
                         filled (Name 0) legible 128 16 goStr $ solid red
      pure errText `_untilEvent_` anyKeydownEvent
      rootLogic
    Right (V2 iw0 ih0, tx) -> do
      let [iw,ih] = map fromIntegral [iw0,ih0]
          title = unwords [ show fp, concat ["(",show iw,"x", show ih, ")"]]
          text (V2 _ h) = move (V2 8 (fromIntegral h) - 8) $ draw $ letters $
                            filled (Name 0) legible 128 16 title $ solid white

          img = draw $ textured tx $
                  fan (0,0) (V2 iw 0,V2 1 0) (V2 iw ih, V2 1 1)
                      [(V2 0 ih, V2 0 1)]
          pic ws = text ws >> img
      (pic <$> windowSize) `_untilEvent_` anyKeydownEvent
  step blank
  (spinner <$> time <*> halfWindowSize) `_untilEvent_` anyKeydownEvent

main :: IO ()
main = do
  (rez,window) <- startupSDL2Backend 800 600 "TileSet Maker v0.0" True
  cfg <- odinConfig
  utc <- getCurrentTime
  let app = (appData cfg rez utc){ appLogic = outputStream rootLogic blank }
  loop window [] app

    where loop window actions app = do
            threadDelay 100
            mapM_ applyAction actions
            events  <- pollEvents >>= mapM (handleEvent . eventPayload)

            wsize <- ctxWindowSize $ rezContext $ appRez app
            newUtc <- getCurrentTime

            let readData  = ReadData { rdWindowSize = uncurry V2 wsize
                                     , rdOdinConfig = appConfig app
                                     }
                stateData = appNextId app
                evs = events ++ [AppEventFrame]
                dt  = realToFrac $ diffUTCTime newUtc $ appUTC app
                ev  = AppEventTime dt
                net = stepMany (appLogic app) evs ev

            ((ui,v),uid,actions1) <- runRWST net readData stateData
            cache <- renderWithSDL2 window (appRez app) (appCache app) ui

            loop window actions1 app{ appNextId = uid
                                    , appLogic = v
                                    , appCache = cache
                                    , appUTC = newUtc
                                    }

