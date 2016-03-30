module App.Framework where

import           Gelatin.SDL2
import           SDL hiding (windowSize, freeCursor)
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Enum as Raw
import qualified SDL.Raw.Types as Raw
import           Linear.Affine (Point(..))
import Control.Monad (when, foldM)
import           Control.Varying
import           Control.Concurrent (threadDelay)
import           Control.Monad.Trans.RWS.Strict
import           Data.Foldable (forM_)
import           Data.Time.Clock
import qualified Data.Map.Strict as M
import qualified Data.Text as Text
import           System.Exit (exitSuccess, exitFailure)
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
applyAction :: AppData -> Action -> IO AppData
applyAction app ActionNone = return app
applyAction app (ActionLog lvl str) = do
  putStrLn $ concat [show lvl, ": ", str]
  return app
applyAction app (ActionSetCursor (CursorPush k)) = do
  let stack = appCursor app
  item <- case M.lookup k stack of
    Nothing -> do cursor <- Raw.createSystemCursor $ cursorToSystem k
                  Raw.setCursor cursor
                  return (1,cursor)
    Just (n,cursor) -> do Raw.setCursor cursor
                          return (n+1,cursor)
  return app{appCursor = M.insert k item stack}
applyAction app (ActionSetCursor (CursorPop k)) = do
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

  return app{appCursor = stack}
applyAction app (ActionSetTextEditing shouldStart) = do
  if shouldStart
    then startTextInput $ Raw.Rect 0 0 100 100
    else stopTextInput
  return app


-- | Handle an incoming SDL2 event payload, generating an in-app event to be
-- provided to the network.
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
handleEvent (TextInputEvent (TextInputEventData _ txt)) =
  return $ AppEventTextInput txt
handleEvent payload = return $ AppEventOther payload

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

-- | Run an FRP sequence representing the entire app logic.
runApp :: String -> AppSequence () -> IO ()
runApp title network = do
  (rez,window) <- startupSDL2Backend 800 600 title True
  fnts <- getFonts
  utc  <- getCurrentTime
  let app = (appData fnts rez utc){ appLogic = outputStream network blank }
  loop window [] app
    where loop window actions app = do
            threadDelay 100
            app1 <- foldM applyAction app actions
            events  <- pollEvents >>= mapM (handleEvent . eventPayload)

            wsize  <- ctxWindowSize $ rezContext $ appRez app1
            P cp   <- snd <$> getModalMouseLocation
            newUtc <- getCurrentTime

            let readData  = ReadData { rdWindowSize = uncurry V2 wsize
                                     , rdCursorPos = fromIntegral <$> cp
                                     , rdFonts = appFonts app1
                                     }
                stateData = appNextId app1
                evs = events ++ [AppEventFrame]
                dt  = realToFrac $ diffUTCTime newUtc $ appUTC app1
                ev  = AppEventTime dt
                net = stepMany (appLogic app1) evs ev

            ((ui,v),uid,actions1) <- runRWST net readData stateData
            cache <- renderWithSDL2 window (appRez app1) (appCache app1) ui

            loop window actions1 app1{ appNextId = uid
                                     , appLogic = v
                                     , appCache = cache
                                     , appUTC = newUtc
                                     }
