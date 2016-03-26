{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Gelatin.SDL2 hiding (get, Event, time, windowSize)
import           Gelatin.Core.Path (pathHasPoint)
import           Linear.Affine (Point(..))
import           Codec.Picture
import           Codec.Picture.Types
import           Control.Varying
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async hiding (race)
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.RWS.Strict
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Hashable
import qualified Data.Map.Strict as M
import           Data.Map (Map)
import qualified Data.IntMap.Strict as IM
import           Data.IntMap (IntMap)
import           Data.Time.Clock
import           Data.Char.FontAwesome
import           Data.Either (either)
import           Data.Foldable (foldl')
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           System.Exit (exitSuccess)
import           Foreign.C.String (peekCString)

import           Odin.Common
import           Odin.GUI

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
              | AppEventKey { keyMotion :: InputMotion
                            , keyRepeat :: Bool
                            , keySym    :: Keysym
                            }
              | AppEventDrop FilePath
              | AppEventMouseMotion (V2 Int) (V2 Int)
              | AppEventMouseButton MouseButton InputMotion (V2 Int)
              | AppEventWheel (V2 Int)
              | AppEventOther EventPayload
              | AppQuit
              deriving (Show, Eq)

data Delta = DeltaTime Float
           | DeltaFrames Int

type AppSequence = SplineT AppEvent (Picture GLuint ()) Effect
type AppSignal = VarT Effect AppEvent

handleEvent :: EventPayload -> IO AppEvent
handleEvent (KeyboardEvent (KeyboardEventData _ m r k)) =
  if isQuit k then exitSuccess else return $ AppEventKey m r k
handleEvent (DropEvent (DropEventData cstr)) =
  AppEventDrop <$> peekCString cstr
handleEvent (MouseWheelEvent (MouseWheelEventData _ _ v)) =
  return $ AppEventWheel (fromIntegral <$> v)
handleEvent (MouseMotionEvent (MouseMotionEventData _ _ _ (P p) v)) =
  return $ AppEventMouseMotion (fromIntegral <$> p) (fromIntegral <$> v)
handleEvent (MouseButtonEvent (MouseButtonEventData _ i _ btn _ (P v))) =
  return $ AppEventMouseButton btn i $ fromIntegral <$> v
handleEvent payload = return $ AppEventOther payload

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

--backHistoryEvent :: Monad m => VarT m AppEvent (Event ())
--backHistoryEvent = mouseButtonEvent

unhandledEvent :: Monad m => VarT m AppEvent (Event EventPayload)
unhandledEvent = var f ~> onJust
  where f (AppEventOther ev) = Just ev
        f _ = Nothing

anyEvent :: Monad m => VarT m AppEvent (Event AppEvent)
anyEvent = var f ~> onJust
  where f AppEventNone = Nothing
        f x = Just x

dropEvent :: Monad m => VarT m AppEvent (Event FilePath)
dropEvent = var f ~> onJust
  where f (AppEventDrop fp) = Just fp
        f _ = Nothing

anyKeydownEvent :: VarT Effect AppEvent (Event (InputMotion, Bool, Keysym))
anyKeydownEvent = var f ~> onJust
  where f (AppEventKey a b c) = Just (a,b,c)
        f _ = Nothing

keyEvent :: Monad m => Int -> InputMotion -> VarT m AppEvent (Event ())
keyEvent scancode motion = var f ~> onTrue
  where f (AppEventKey m _ Keysym{keysymScancode = code}) =
          (Scancode $ fromIntegral scancode) == code && motion == m
        f _ = False

escCode :: Int
escCode = 41

enterCode :: Int
enterCode = 40

cancelKeyEvent :: Monad m => VarT m AppEvent (Event ())
cancelKeyEvent = keyEvent escCode Released

commitKeyEvent :: Monad m => VarT m AppEvent (Event ())
commitKeyEvent = keyEvent enterCode Released

mouseWheelEvent :: Monad m => VarT m AppEvent (Event (V2 Int))
mouseWheelEvent = var f ~> onJust
  where f (AppEventWheel v) = Just v
        f _ = Nothing

anyMouseButtonEvent :: Monad m => VarT m AppEvent (Event (MouseButton, InputMotion, V2 Int))
anyMouseButtonEvent = var f ~> onJust
  where f (AppEventMouseButton btn action v) = Just (btn,action,v)
        f _ = Nothing

mouseButtonEvent :: Monad m
           => MouseButton -> InputMotion -> VarT m AppEvent (Event (V2 Int))
mouseButtonEvent btn action = anyMouseButtonEvent ~> var f ~> onJust
  where f (Event (btn0,action0,v)) =
          if btn0 == btn && action0 == action
          then Just v
          else Nothing
        f _ = Nothing

-- | A stream of mouse motion events. `fst` contains the new mouse position
-- while `snd` contains the relative vector since the last event.
mouseMotionEvent :: Monad m => VarT m AppEvent (Event (V2 Int, V2 Int))
mouseMotionEvent = var f ~> onJust
  where f (AppEventMouseMotion p v) = Just (p,v)
        f _ = Nothing

-- | A stream of relative motion of th mouse.
mouseMoveEvent :: Monad m => VarT m AppEvent (Event (V2 Float))
mouseMoveEvent = fmap (fmap fromIntegral <$> snd) <$> mouseMotionEvent

-- | A stream of absolute mouse positions.
mousePositionEvent :: Monad m => VarT m AppEvent (Event (V2 Float))
mousePositionEvent = fmap (fmap fromIntegral . fst) <$> mouseMotionEvent

-- | The last mouse position. After one mouse position event has proc'd this
-- will always produce an event.
lastMousePositionEvent :: Monad m => VarT m AppEvent (Event (V2 Float))
lastMousePositionEvent =
  mousePositionEvent ~> accumulate (\x y -> if isEvent y then y else x) NoEvent

mouseIsOver :: (Monad m, Integral t)
            => (V2 t, V2 t) -> VarT m AppEvent (Event ())
mouseIsOver (tl,br) = lastMousePositionEvent ~> var f
  where bounds = (fromIntegral <$> tl, fromIntegral <$> br)
        f ev = case (`pointInBounds` bounds) <$> ev of
                 Event True -> Event ()
                 _ -> NoEvent

mouseIsOut :: (Monad m, Integral t)
           => (V2 t, V2 t) -> VarT m AppEvent (Event ())
mouseIsOut bounds = mouseIsOver bounds ~> var (not . isEvent) ~> onTrue

windowSize :: VarT Effect a (V2 Int)
windowSize = varM $ const $ asks rdWindowSize

halfWindowSize :: VarT Effect a (V2 Float)
halfWindowSize = (fmap fromIntegral <$> windowSize) / 2

time :: Monad m => VarT m AppEvent Float
time = var f ~> onJust ~> foldStream (+) 0
  where f (AppEventTime dt) = Just dt
        f _ = Nothing

frames :: Monad m => VarT m AppEvent Int
frames = var f ~> accumulate (+) 0
  where f AppEventFrame = 1
        f _ = 0

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

data TileSetMaker = TileSourcePicker (Picker TileSource)
                  | TileSourceEditor TileSource

data LoadedPic = LoadedPic { lpTex   :: GLuint
                           , lpSize  :: V2 Int
                           , lpPos   :: V2 Int
                           , lpScale :: Float
                           }

loadedPicHitArea :: LoadedPic -> (V2 Int, V2 Int)
loadedPicHitArea lp = (floor <$> tl, floor <$> br)
  where z = s *^ (fromIntegral <$> lpSize lp)
        tl = fromIntegral <$> lpPos lp
        s = lpScale lp
        br = tl + z

drawLoadedPic :: LoadedPic -> Picture GLuint ()
drawLoadedPic lp = do
  let V2 w h = fromIntegral <$> lpSize lp
      p = fromIntegral <$> lpPos lp
      s = V2 (lpScale lp) (lpScale lp)
      t = lpTex lp
  move p $ scale s $ withTexture t $
      fan (0,0) (V2 w 0,V2 1 0) (V2 w h, V2 1 1) [(V2 0 h, V2 0 1)]

dragPic :: LoadedPic -> V2 Int -> AppSequence LoadedPic
dragPic lp offset = do
  let mouseUp = mouseButtonEvent ButtonLeft Released
      lp1 = lp{ lpPos = lpPos lp + offset }
  (p,e) <- pure (drawLoadedPic lp1) `untilEvent` eitherE mouseUp mouseMotionEvent
  case e of
    Left _ -> transformPic lp{ lpPos = lpPos lp + offset }
    Right (_,v) -> step p >> dragPic lp (offset + v)

transformPic :: LoadedPic -> AppSequence LoadedPic
transformPic lp = do
  let scaleEvent :: Monad m => VarT m AppEvent (Event Float)
      scaleEvent = fmap (\(V2 _ y) -> fromIntegral y) <$> mouseWheelEvent
      clickEvent = mouseButtonEvent ButtonLeft Pressed
      pic = drawLoadedPic lp
  step pic
  e <- pure pic `_untilEvent` eitherE clickEvent scaleEvent
  case e of
    Left _ -> dragPic lp 0
    Right s -> transformPic lp{ lpScale = s/20 + lpScale lp }

data TileSource = TileSource { tsrcFilePath :: FilePath
                             , tsrcPic :: LoadedPic
                             , tsrcTiles :: IntMap (V2 Int)
                             }

drawTileSource :: FontData -> TileSource -> Picture GLuint ()
drawTileSource fnt ts = do
  drawLoadedPic $ tsrcPic ts
  let V2 _ h = fromIntegral <$> lpSize (tsrcPic ts)
      V2 x y = fromIntegral <$> lpPos (tsrcPic ts)
  move (V2 x (y+h+8)) $ withLetters $
    filled (Name 0) fnt 128 8 (tsrcFilePath ts) $ solid white

data MarkupScreen = MarkupScreen { msTileSources :: IntMap TileSource }

drawMarkupScreen :: FontData -> MarkupScreen -> Picture GLuint ()
drawMarkupScreen fnt =
    mapM_ (drawTileSource fnt . snd) . IM.toList . msTileSources

iconButtonPic :: FontData -> V4 Float -> V2 Float -> Float -> Char -> Picture a ()
iconButtonPic fnt color tl s ch = move (tl + V2 0 s) $ withLetters $
  filled (Name $ hash color) fnt 72 s [ch] $ solid color

iconButtonDown :: V2 Float -> Float -> Char -> AppSequence ()
iconButtonDown tl s ch = do
  icons <- lift getIconFont
  let pic = iconButtonPic icons gray tl s ch
      bounds = pictureBounds pic
  up <- pure pic `_untilEvent` mouseButtonEvent ButtonLeft Released
  step pic
  unless (pointInBounds (fromIntegral <$> up) bounds) $ iconButton tl s ch

iconButton :: V2 Float -> Float -> Char -> AppSequence ()
iconButton tl s ch = do
  icons <- lift getIconFont
  let pic = iconButtonPic icons white tl s ch
      bounds = (tl,tl + V2 s s)

  down <- pure pic `_untilEvent` mouseButtonEvent ButtonLeft Pressed
  step pic

  if pointInBounds (fromIntegral <$> down) bounds
    then iconButtonDown tl s ch
    else iconButton tl s ch

data TileSourceEdit = TSEditMove
                    | TSEditScale
                    | TSEditAdd
                    | TSEditSub
                    deriving (Show, Eq)

tileSourceEditBar :: AppSequence TileSourceEdit
tileSourceEditBar = mapOutput ((\ws -> (bar ws <>)) <$> windowSize) btns
  where moveBtn  = iconButton (V2 4 0) 32 faArrows
        scaleBtn = iconButton (V2 4 32) 32 faArrowsAlt
        addBtn   = iconButton (V2 4 64) 32 faPlusSquare
        subBtn   = iconButton (V2 4 96) 32 faMinusSquare
        bar (V2 _ h) = withColor $ rectangle 0 (V2 36 $ fromIntegral h) $
                         const (V4 1 1 1 0.3)
        btns = raceMany [ TSEditMove  <$ moveBtn
                        , TSEditScale <$ scaleBtn
                        , TSEditAdd   <$ addBtn
                        , TSEditSub   <$ subBtn
                        ]

-- | A signal that procs 'Left ()' when the escape key is hit or 'Right ()' when
-- the enter key is hit.
commitOrCancelKeyEvent :: Monad m => VarT m AppEvent (Event (Either () ()))
commitOrCancelKeyEvent = eitherE cancl commit
  where cancl = keyEvent 41 Released
        commit = keyEvent 40 Released

moveTileSource :: TileSource -> AppSequence TileSource
moveTileSource ts = do
  fnt <- lift getLegibleFont

  let movingPic v = move v $ withAlpha 0.5 $ drawTileSource fnt ts
      mouseDown = mouseButtonEvent ButtonLeft Pressed
  e <- pure (movingPic 0) `_untilEvent` eitherE mouseDown commitKeyEvent
  case e of
    Right _ -> return ts
    Left _  -> do
      let offset :: AppSignal (V2 Float)
          offset   = mouseMoveEvent ~> foldStream (^+^) 0 ~> vstrace "offset: "
          mouseUp  = mouseButtonEvent ButtonLeft Released
          endEvent = ((<$) <$> offset) <*> mouseUp

      v1 <- (movingPic <$> offset) `_untilEvent` endEvent
      let ts1 = ts{ tsrcPic = lp{ lpPos = p + (floor <$> v1) }}
          lp = tsrcPic ts
          p = lpPos lp
      moveTileSource ts1

editTileSourceWith :: TileSource -> TileSourceEdit -> AppSequence TileSource
editTileSourceWith ts TSEditMove = do
  fnt <- lift getLegibleFont
  let pic = drawTileSource fnt ts
  e <- race (<>) (pure pic `_untilEvent` cancelKeyEvent)
                (moveTileSource ts)
  step pic
  return $ case e of
    Left _ -> ts
    Right ts1 -> ts1

  --let hitarea = loadedPicHitArea $ tsrcPic ts
  --    (a,b) = hitarea
  --    (V2 x1 y1, V2 x2 y2) = (fromIntegral <$> a, fromIntegral <$> b)
  --    tsPic = drawTileSource fnt ts
  --    tsOutline = withStroke [StrokeWidth 4, StrokeFeather 1] $
  --                  lineStart (V2 x1 y1, white) $ do lineTo (V2 x2 y1, white)
  --                                                   lineTo (V2 x2 y2, white)
  --                                                   lineTo (V2 x1 y2, white)
  --                                                   lineTo (V2 x1 y1, white)
  --    tsHover = tsPic >> tsOutline
  --    mouseDown = mouseButtonEvent ButtonLeft Pressed
  --pure tsPic `_untilEvent_` mouseIsOver hitarea
  --e <- pure tsHover `_untilEvent` eitherE (mouseIsOut hitarea) mouseDown

  --step tsHover

  --case e of
  --  Left () -> editTileSourceWith ts TSEditMove
  --  Right _ -> moveTileSource ts

editTileSourceWith ts edit = do
  infoStr $ show edit
  fnt <- lift getLegibleFont
  pure (drawTileSource fnt ts) `_untilEvent_` never
  return ts

editTileSource :: TileSource -> AppSequence TileSource
editTileSource ts = do
  legible <- lift getLegibleFont
  (mpic, ebtn) <- capture $ race (<>)
    (pure (drawTileSource legible ts) `_untilEvent_` keyEvent 41 Released)
    tileSourceEditBar

  let pic = fromMaybe blank mpic
  step pic

  case ebtn of
    Right btn -> editTileSourceWith ts btn >>= editTileSource
    Left () -> return ts

getFirstTileSource :: AppSequence TileSource
getFirstTileSource = do
  fancy <- lift getFancyFont
  legible <- lift getLegibleFont

  let text = "Drag and drop an image to start a new TileSource."
      instructions = move (V2 0 32) $ draw $ letters $
                       filled (Name 0) fancy 128 32 text $ solid white

  fp <- pure instructions `_untilEvent` dropEvent

  infoStr $ "Dropped file " ++ fp

  eimg <- (spinner <$> time <*> halfWindowSize) `_untilEvent` reqImage fp
  infoStr $ "Got file " ++ fp
  case eimg of
    Left err -> do
      let errText = do let errStr = show err
                           goStr  = "Press any key to continue."
                       move 16 $ draw $ letters $
                         filled (Name 0) legible 128 16 errStr $ solid red
                       move 32 $ draw $ letters $
                         filled (Name 0) legible 128 16 goStr $ solid red
      pure errText `_untilEvent_` anyKeydownEvent
      getFirstTileSource
    Right (size, tx) -> do
      wsize0 <- lift $ asks rdWindowSize
      let wsize = fromIntegral <$> wsize0 :: V2 Float
          sz = fromIntegral <$> size :: V2 Float
          lp = LoadedPic tx size (floor <$> (wsize/2 - sz/2)) 1
      editTileSource $ TileSource fp lp mempty

markupScreen :: AppSequence MarkupScreen
markupScreen = do
  ts <- getFirstTileSource
  infoStr "Got first TileSource"
  let ms = MarkupScreen $ IM.singleton 0 ts
  fnt <- lift getLegibleFont
  pure (drawMarkupScreen fnt ms) `untilEvent` anyKeydownEvent
  return ms
  --(spinner <$> time <*> halfWindowSize) `_untilEvent_` anyKeydownEvent

rootLogic :: AppSequence ()
rootLogic = do
  fnt <- lift getLegibleFont
  let btn = Button "This is a button." fnt 16 ButtonStateUp
  pure (move 100 $ paintButton btn) `_untilEvent_` never
  markupScreen
  step blank
  return ()

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

