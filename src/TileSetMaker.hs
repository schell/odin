{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Gelatin.SDL2 as SDL2 hiding (get, Event)
import           Control.Varying
import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.RWS.Strict
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)
import           System.Exit (exitSuccess)
import           Foreign.C.String (peekCString)

import           Odin.Common

newtype Uid = Uid { unUid :: Int } deriving (Show, Eq, Ord, Enum, Num)

data Action = ActionSetPicRenderer Uid (OdinConfig -> Picture ())
            | ActionSetImageRenderer Uid FilePath
            | ActionDeleteRenderer Uid
            | ActionSetTransform Uid (Maybe Transform)
            | ActionNone

data AppError = AppImageLoadError Uid FilePath deriving (Show, Eq, Ord)

type Effect = RWS () [Action] Uid

data AppData = AppData { appNextId    :: Uid
                       , appLogic     :: VarT Effect AppEvent ()
                       , appTransforms:: IntMap Transform
                       , appRenderers :: IntMap (Renderer IO Transform)
                       , appConfig    :: OdinConfig
                       , appRez       :: Rez
                       , appErrors    :: [AppError]
                       }

data AppEvent = AppEventNone
              | AppKeyEvent { keyMotion :: InputMotion
                            , keyRepeat :: Bool
                            , keySym    :: Keysym
                            }
              | AppDropEvent FilePath
              | AppErrorEvent AppError
              | AppQuit
              deriving (Show, Eq, Ord)

handleEvent :: EventPayload -> IO AppEvent
handleEvent (KeyboardEvent (KeyboardEventData _ m r k)) =
  if isQuit k then exitSuccess else return $ AppKeyEvent m r k
handleEvent (DropEvent (DropEventData cstr)) =
  AppDropEvent <$> peekCString cstr
handleEvent _ = return AppEventNone

applyAction :: AppData -> Action -> IO AppData
applyAction app ActionNone = return app
applyAction app (ActionDeleteRenderer (Uid uid)) =
  case IM.lookup uid $ appRenderers app of
    Nothing -> do putStrLn $ "Could not clean renderer for " ++ show (Uid uid)
                  return app
    Just r  -> do fst r
                  return app{ appRenderers = IM.delete uid $ appRenderers app }
applyAction app (ActionSetPicRenderer (Uid uid) f) = do
  r <- compileRenderer (appRez app) $ f (appConfig app)
  return $ app{ appRenderers = IM.insert uid r $ appRenderers app }
applyAction app (ActionSetImageRenderer (Uid uid) fp) = do
  mimg <- loadImage fp
  case mimg of
    Nothing  -> do putStrLn $ "Could not load the image " ++ show fp
                   let errs = appErrors app ++ [AppImageLoadError (Uid uid) fp]
                   return app { appErrors = errs }
    Just (V2 w h, tex) -> do
      let [w',h'] = map fromIntegral [w,h]
      rs <- mapM (texturePrimsRenderer $ appRez app) $ collectPrims $
        fan (V2 0 0, V2 0 0) (V2 w' 0, V2 1 0) (V2 w' h', V2 1 1)
            [(V2 0 h', V2 0 1)]
      let (c,r) = foldl appendRenderer emptyRenderer rs
          f t   = bindTexAround tex $ r t
          render= (c,f)
      return app{ appRenderers = IM.insert uid render $ appRenderers app }
applyAction app (ActionSetTransform (Uid uid) (Just t)) =
  return app{ appTransforms = IM.insert uid t $ appTransforms app }
applyAction app (ActionSetTransform (Uid uid) Nothing) =
  return app{ appTransforms = IM.delete uid $ appTransforms app }

runEvent :: AppData -> AppEvent -> IO AppData
runEvent app ev = do
  let (((),v),uid,actions) = runRWS (runVarT (appLogic app) ev) () (appNextId app)
  foldM applyAction app{ appNextId = uid, appLogic = v } actions

renderApp :: Window -> AppData -> IO ()
renderApp window app = do
  let transforms = appTransforms app
      renderers  = snd <$> appRenderers app
      tfs = IM.elems $ IM.intersectionWith (,) transforms renderers
  clearFrame $ appRez app
  mapM_ (\(t,r) -> r t) tfs
  updateWindowSDL2 window
  threadDelay 100

fresh :: SplineT a b Effect Uid
fresh = lift $ do
  uid <- get
  modify (+1)
  return uid

dropEvent :: Monad m => VarT m AppEvent (Event FilePath)
dropEvent = var f ~> onJust
  where f (AppDropEvent fp) = Just fp
        f _ = Nothing

errorEvent :: Monad m => VarT m AppEvent (Event AppError)
errorEvent = var f ~> onJust
  where f (AppErrorEvent err) = Just err
        f _ = Nothing

anyKeydownEvent :: Monad m => VarT m AppEvent (Event ())
anyKeydownEvent = var f ~> onTrue
  where f (AppKeyEvent Pressed r k) = True
        f _ = False

-- | Wait indefinitely for an event to occur.
waitForEvent :: Monad m => VarT m a (Event b) -> SplineT a () m b
waitForEvent ev = pure () `_untilEvent` ev

-- | Wait a specific number of frames for an event to occur.
waitForEventFor :: Monad m
                => Int -> VarT m a (Event b) -> SplineT a () m (Maybe b)
waitForEventFor n ev = do
  e <- race mappend (waitForEvent ev) (pure () `untilEvent_` (1 ~> after (n + 2)))
  return $ case e of
    Left err -> Just err
    Right () -> Nothing

renderPic :: (OdinConfig -> Picture ()) -> SplineT AppEvent () Effect Uid
renderPic f = do
  uid <- fresh
  lift $ tell [ ActionSetPicRenderer uid f
              , ActionSetTransform uid $ Just mempty
              ]
  step ()
  return uid

renderImage :: FilePath -> SplineT AppEvent () Effect Uid
renderImage fp = do
  uid <- fresh
  lift $ tell [ ActionSetImageRenderer uid fp
              , ActionSetTransform uid $ Just mempty
              ]
  step ()
  return uid

deleteEntity :: Uid -> SplineT AppEvent () Effect ()
deleteEntity uid = do
  lift $ tell [ActionDeleteRenderer uid, ActionSetTransform uid Nothing]
  step ()

rootLogic :: SplineT AppEvent () Effect ()
rootLogic = do
  pic <- renderPic $ \cfg -> withFont (ocFancyFont cfg) $
    withFill (solid white) $ move 32 $
      letters 128 32 "Drag an image to the window to start a new tileset."

  fp <- pure () `_untilEvent` dropEvent
  deleteEntity pic
  img  <- renderImage fp
  merr <- waitForEventFor 1 errorEvent
  case merr of
    Just err -> do
      errPic <- renderPic $ \cfg -> withFont (ocLegibleFont cfg) $
        withFill (solid red) $ do
          move 16 $ letters 128 16 $ show err
          move 32 $ letters 128 16 "Press any key to continue."
      waitForEvent anyKeydownEvent
      deleteEntity errPic
      rootLogic
    Nothing -> do
      title <- renderPic $ \cfg -> withFont (ocLegibleFont cfg) $
        withFill (solid white) $
          move 16 $ letters 128 16 $ show fp
      return ()
  return ()

main :: IO ()
main = do
  (rez,window) <- startupSDL2Backend 800 600 "TileSet Maker v0.0" True
  cfg <- odinConfig
  loop window $ AppData 1 (outputStream () rootLogic) mempty mempty cfg rez []
    where loop window app = do
            let errs = map AppErrorEvent $ appErrors app
            events <- pollEvents >>= mapM (handleEvent . eventPayload)
            app' <- foldM runEvent app{ appErrors = []} $ errs ++ events
            renderApp window app'
            loop window app'

