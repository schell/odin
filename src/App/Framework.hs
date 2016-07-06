{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module App.Framework where

import           Gelatin.SDL2
import           SDL hiding (windowSize, freeCursor)
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Enum as Raw
import qualified SDL.Raw.Types as Raw
import           Codec.Picture (readImage)
import           Linear.Affine (Point(..))
import           Control.Monad (when, foldM)
import           Control.Varying
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Trans.RWS.Strict
import           Data.Foldable (forM_)
import           Data.Functor.Identity
import           Data.Time.Clock
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable.Mutable as MV
import           Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector as V
import           System.Exit (exitFailure)
import           System.FilePath
import           System.Directory
import           Foreign.C.String (peekCString)

import App.Control.Monad

isQuit :: Keysym -> Bool
isQuit (Keysym (Scancode 20) (Keycode 113) m) = any ($ m)
    [ keyModifierLeftCtrl
    , keyModifierRightCtrl
    , keyModifierLeftGUI
    , keyModifierRightGUI
    ]
isQuit _ = False

cursorToSystem :: CursorType -> Raw.SystemCursor
cursorToSystem CursorHand = Raw.SDL_SYSTEM_CURSOR_HAND

-- | Apply an action to the outside world and optionally to the app itself.
applyAction :: (AppData a, [AppEvent]) -> Action -> IO (AppData a, [AppEvent])
applyAction acc ActionNone = return acc
applyAction acc (ActionLog lvl str) = do
  putStrLn $ concat [show lvl, ": ", str]
  return acc
applyAction (app,evs) (ActionSetCursor (CursorPush k)) = do
  let stack = appCursor app
  item <- case M.lookup k stack of
    Nothing -> do cursor <- Raw.createSystemCursor $ cursorToSystem k
                  Raw.setCursor cursor
                  return (1,cursor)
    Just (n,cursor) -> do Raw.setCursor cursor
                          return (n+1,cursor)
  return (app{appCursor = M.insert k item stack}, evs)
applyAction (app,evs) (ActionSetCursor (CursorPop k)) = do
  let (mcursor,n) = case M.lookup k $ appCursor app of
                  Nothing -> (Nothing, 0)
                  Just (1, cursor) -> (Just cursor, 0)
                  Just (x, _) -> (Nothing, x-1)
      stack = if n == 0
                then M.delete k $ appCursor app
                else M.adjust (\(_,cursor) -> (n,cursor)) k $ appCursor app

  when (M.size stack == 0) $ do
    cursor <- Raw.createSystemCursor Raw.SDL_SYSTEM_CURSOR_ARROW
    Raw.setCursor cursor

  forM_ mcursor Raw.freeCursor

  return (app{appCursor = stack}, evs)
applyAction (app,evs) (ActionSetTextEditing shouldStart) = do
  if shouldStart
    then startTextInput $ Raw.Rect 0 0 100 100
    else stopTextInput
  return (app, evs)
applyAction (app,evs) (ActionLoadImage (Uid k) fp) = do
  a <- async $ readImage fp
  return (app{ appAsyncs = IM.insert k (AsyncThreadImageLoad a) $ appAsyncs app }
         ,evs
         )
applyAction (app,evs) (ActionLoadRenderer uid io) = do
  r <- io $ appRez app
  return (app, evs ++ [AppEventLoadedRenderer uid r])
applyAction acc (ActionUnloadRenderer r) = fst r >> return acc

-- | Check up on all our async tasks and possibly conclude them, generating
-- an action.
checkAsyncs :: AppData a -> IO ([AppEvent], AppData a)
checkAsyncs app = do
  (evs,as1) <- (\f -> foldM f ([],mempty) $ IM.toList $ appAsyncs app) $
    \(evs, as) (k, t@(AsyncThreadImageLoad a)) ->
      poll a >>= \case
    Nothing -> return (evs, IM.insert k t as)
    Just e  -> do
      event <- case e of
        Left exc -> return $ AppEventLoadImage (Uid k) $ Left (show exc)
        Right (Left str) -> return $ AppEventLoadImage (Uid k) $ Left str
        Right (Right dyn) ->
          fmap (AppEventLoadImage (Uid k) . Right) (loadTexture dyn)
      return (evs ++ [event], IM.delete k as)
  return (evs, app{ appAsyncs = as1 })

-- | Handle an incoming SDL2 event payload, generating an in-app event to be
-- provided to the network.
handleEvent :: EventPayload -> IO AppEvent
handleEvent (KeyboardEvent (KeyboardEventData _ m r k)) =
  return $ if isQuit k then AppQuit else AppEventKey m r k
handleEvent (DropEvent (DropEventData cstr)) =
  AppEventDrop <$> peekCString cstr
handleEvent (MouseWheelEvent (MouseWheelEventData _ _ v)) =
  return $ AppEventWheel (fromIntegral <$> v)
handleEvent (MouseMotionEvent (MouseMotionEventData _ _ _ (P p) v)) =
  return $ AppEventMouseMotion (fromIntegral <$> p) (fromIntegral <$> v)
handleEvent (MouseButtonEvent (MouseButtonEventData _ i _ btn _ (P v))) =
  return $ AppEventMouseButton btn i $ fromIntegral <$> v
handleEvent (TextInputEvent (TextInputEventData _ txt)) =
  return $ AppEventTextInput txt
handleEvent (JoyDeviceEvent (JoyDeviceEventData iid)) = do
  vjoys <- availableJoysticks
  let fjoys = V.filter ((== iid) . fromIntegral . joystickDeviceId) vjoys
  if V.length fjoys >= 1
    then do j <- openJoystick $ V.head fjoys
            jid <- getJoystickID j
            return $ AppEventJoystickAdded jid
    -- | Totally not sure about this here. We probably need to keep a list
    -- of opened joysticks so we can close them here, but that would require
    -- updating handleEvent to modify AppData.
    else return $ AppEventJoystickRemoved iid
handleEvent (JoyAxisEvent (JoyAxisEventData jid axis val)) =
  return $ AppEventJoystickAxis jid axis val
handleEvent (JoyBallEvent (JoyBallEventData jid ball rel)) =
  return $ AppEventJoystickBall jid ball rel
handleEvent (JoyHatEvent (JoyHatEventData jid hat val)) =
  return $ AppEventJoystickHat jid hat val
handleEvent (JoyButtonEvent (JoyButtonEventData jid btn st)) =
  return $ AppEventJoystickButton jid btn st
handleEvent !payload = return $ AppEventOther payload

-- | Load our standard fonts.
getFonts :: IO Fonts
getFonts = do
    -- Get our fonts
    assets <- (</> "assets") <$> getCurrentDirectory
    -- Load our header font
    let fonts = assets </> "fonts"
    ed <- loadFont $ fonts </> "Deutsch.ttf"
    eh <- loadFont $ fonts </> "Hack-Regular.ttf"
    ef <- loadFont $ fonts </> "fontawesome-webfont.ttf"
    case Fonts <$> ed <*> eh <*> ef of
        Left err -> do print err
                       exitFailure
        Right fnts -> return fnts

type AudioCallback t = AudioFormat t -> IOVector t -> IO ()

deviceSpec :: (forall t. AudioFormat t -> IOVector t -> IO ()) -> OpenDeviceSpec
deviceSpec cb = OpenDeviceSpec
  { openDeviceFreq     = Desire 44100
  , openDeviceFormat   = Desire FloatingNativeAudio
  , openDeviceChannels = Desire Mono
  , openDeviceSamples  = 1024
  , openDeviceCallback = cb
  , openDeviceUsage    = ForPlayback
  , openDeviceName     = Nothing
  }

sinWave :: Float -> Float -> Float -> Var Float Float
sinWave amp freq phase = accumulate (+) 0 ~> var (\t -> amp * sin (phase + 2 * pi * freq * t))

audioNetwork :: Var Float Float
audioNetwork = 0--sinWave 1 440 0

audioCB :: forall t. TVar (Float, Var Float Float) -> AudioFormat t
        -> IOVector t -> IO ()
audioCB ref FloatingLEAudio buffer = audioCB ref FloatingNativeAudio buffer
audioCB ref FloatingNativeAudio buffer = do
  (dt,v) <- readTVarIO ref
  let numSamples = MV.length buffer
      dts = replicate numSamples dt
      ndxs = take numSamples [0 .. ]
      fill v1 (t,n) = do
        let Identity (sample, v2) = runVarT v1 t
        MV.write buffer n sample
        return v2
  v1 <- foldM fill v (zip dts ndxs)
  atomically $ writeTVar ref (dt,v1)

audioCB _ t _ = putStrLn $ "Unsupported audio type '" ++ show t ++ "'"

exitApp :: AudioDevice -> IO ()
exitApp device = do
  closeAudioDevice device
  quit

type AppRender a = Window -> AppData a -> a -> IO (AppData a)

picAppRender :: Window -> AppData Pic -> Pic -> IO (AppData Pic)
picAppRender window app ui = do
  cache <- renderWithSDL2 window (appRez app) (appCache app) ui
  return app{ appCache = cache }

-- | Run an FRP sequence representing the entire app logic.
runApp :: AppRender a -> AppSignal a -> String -> IO ()
runApp rndr network title = do
  (rez,window)  <- startupSDL2Backend 800 600 title True
  ref <- newTVarIO (0,audioNetwork)
  (device,spec) <- openAudioDevice $ deviceSpec $ audioCB ref
  let dt = 1 / fromIntegral (audioSpecFreq spec)
  atomically $ writeTVar ref (dt, audioNetwork)
  putStrLn $ "Frequency: " ++ show (audioSpecFreq spec)
  putStrLn $ "Channels: " ++ show (audioSpecChannels spec)
  putStrLn $ "Silence: " ++ show (audioSpecSilence spec)
  putStrLn $ "Spec Size: " ++ show (audioSpecSize spec)
  setAudioDevicePlaybackState device Play

  names <- getAudioDeviceNames ForPlayback
  print names
  fnts <- getFonts
  utc  <- getCurrentTime
  let app = appData network fnts rez utc device
  loop window [] app
    where loop window actions app = do
            threadDelay 1
            (aEvs,app1) <- checkAsyncs app
            (app2,aEvs2)<- foldM applyAction (app1,[]) actions
            events      <- pollEvents >>= mapM (handleEvent . eventPayload)
            wsize       <- ctxWindowSize $ rezContext $ appRez app2
            P cp        <- snd <$> getModalMouseLocation
            newUtc      <- getCurrentTime

            let readData  = ReadData { rdWindowSize = uncurry V2 wsize
                                     , rdCursorPos = fromIntegral <$> cp
                                     , rdFonts = appFonts app2
                                     }
                stateData = appNextId app2
                allEvs = aEvs ++ aEvs2
                evs = allEvs ++ events ++ [AppEventFrame]
                dt  = realToFrac $ diffUTCTime newUtc $ appUTC app2
                ev  = AppEventTime dt
                net = stepMany (appLogic app2) evs ev
                ((ui,v),!uid,actions1) = runRWS net readData stateData

            app3 <- rndr window app1 ui
            let f AppQuit = True
                f _ = False
            if any f events
              then exitApp (appAudio app3)
              else loop window actions1 app3{ appNextId = uid
                                            , appLogic = v
                                            , appUTC = newUtc
                                            }
