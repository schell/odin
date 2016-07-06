{-# LANGUAGE RecordWildCards #-}
import           Gelatin.Core
import           Gelatin.Picture
import           Gelatin.SDL2
import           Gelatin.GL
import           SDL
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import           Data.Word (Word32)
import           Data.Functor.Identity
import           Control.Monad (void, forever, when)
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Varying
import           Control.Concurrent (threadDelay)
import           System.Exit (exitSuccess)

import           App.Framework (isQuit)

data Update = UpdateTransform Uid PictureTransform

data Input = InputTime Float
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
data Components = Components { compRndring  :: IntMap GLRenderer
                             , compTrnsfrm  :: IntMap PictureTransform
                             , compBehavior :: IntMap (Var Input Update)
                             }

emptyComponents :: Components
emptyComponents = Components mempty mempty mempty
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

tickBehaviors :: Float -> System [Update]
tickBehaviors dt = do
  c <- gets sysComponents
  let m = compBehavior c
      run v = runIdentity $ runVarT v $ InputTime dt
      steps :: IntMap (Update, Var Input Update)
      steps = run <$> m
      updates = fst <$> steps
      nextbs = snd <$> steps
  modify' $ \s -> s{sysComponents = c{compBehavior = nextbs}}
  return $ map snd $ IM.toList updates

tickUpdates :: [Update] -> System ()
tickUpdates = mapM_ update
  where update (UpdateTransform uid t) = addComponentTransform uid t

addComponentRendering :: Uid -> GLRenderer -> System ()
addComponentRendering (Uid uid) r = do
  c <- gets sysComponents
  let m = compRndring c
  modify' $ \s -> s{sysComponents = c{compRndring = IM.insert uid r m }}

addComponentTransform :: Uid -> PictureTransform -> System ()
addComponentTransform (Uid uid) p = do
  c <- gets sysComponents
  let m = compTrnsfrm c
  modify' $ \s -> s{sysComponents = c{compTrnsfrm = IM.insert uid p m }}

addComponentBehavior :: Uid -> Var Input Update -> System ()
addComponentBehavior (Uid uid) v = do
  c <- gets sysComponents
  let m = compBehavior c
  modify' $ \s -> s{sysComponents = c{compBehavior = IM.insert uid v m }}

deltaTime :: Var Input Float
deltaTime = var f
  where f (InputTime t) = t
        --f _ = 0

setupNetwork :: System ()
setupNetwork = do
  master <- freshUid
  rez    <- gets sysRez
  r      <- liftIO $ do
    pr <- compilePictureRenderer rez mempty $ withColor $ rectangle 10 20 $ const red
    return $ fst pr
  master `addComponentTransform` mempty
  master `addComponentRendering` r
  let u = UpdateTransform master <$> b
      b = PictureTransform <$> t <*> pure 1 <*> pure 1
      t = Transform <$> (deltaTime ~> (V2 <$> x <*> y)) <*> 1 <*> 0
      x :: Var Float Float
      x = outputStream dx 0
      dx = do tween easeInExpo 10 100 0.25 >>= (`constant` 0.25)
              tween easeInExpo 100 10 0.25 >>= (`constant` 0.25)
              dx
      y :: Var Float Float
      y = outputStream dy 0
      dy = do constant 10 0.25 >> tween easeInExpo 10 100 0.25
              constant 100 0.25 >> tween easeInExpo 100 10 0.25
              dy
  master `addComponentBehavior` u
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
processEvent :: EventPayload -> IO ()
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
    forever $ do
      liftIO $ void $ pollEvents >>= mapM (processEvent . eventPayload)
      tickTime >>= tickBehaviors >>= tickUpdates
      renderWith rez window
      liftIO $ threadDelay 1
