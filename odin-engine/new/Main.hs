{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main where

import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.STM.TVar  (TVar, modifyTVar', newTVar,
                                               readTVarIO, writeTVar)
import           Control.Lens                 hiding (to, transform)
import           Control.Monad                (guard, void, when)
import           Control.Monad.Fix            (fix)
import           Control.Monad.STM            (atomically)
import           Control.Varying              hiding (Event, never)
import           Data.Default                 (def)
import           Data.Monoid                  ((<>))
import qualified Data.Vector.Unboxed          as V
import           Data.Word                    (Word32)
import           Gelatin.GL
import           Reflex.SDL2                  hiding (fan, rotate)
import           System.Exit                  (exitSuccess)

import           Data.Char.FontAwesome        (faAnchor, faCheckSquareO,
                                               faSquareO)

import           Odin.Engine.New
import           Odin.Engine.New.EventT
import           Odin.Engine.New.UI.Animation
import           Odin.Engine.New.UI.Button
import           Odin.Engine.New.UI.Checkbox
import           Odin.Engine.New.UI.Configs
import           Odin.Engine.New.UI.Layer
import           Odin.Engine.New.UI.Layout
import           Odin.Engine.New.UI.Picture
import           Odin.Engine.New.UI.TextField
import           Odin.Engine.New.UI.TextInput


subguest :: forall r t m. OdinWidget r t m => m ()
subguest = do
  ------------------------------------------------------------------------------
  -- First we'll draw a colorful background that always stays anchored at the
  -- top left corner, but also stretces to the boundaries of the window.
  ------------------------------------------------------------------------------
  let pic = setGeometry $ fan $ do
              to (     0, V4 1 1 0 1)
              to (V2 1 0, V4 0 1 1 1)
              to (     1, V4 1 0 1 1)
              to (V2 0 1, V4 1 1 1 1)
  evWindowSize <- getWindowSizeEvent
  let evTfrm = pure . scaleV2 . fmap fromIntegral <$> evWindowSize
  colorPicture pic [] $ def & setTransformEvent .~ evTfrm
  ------------------------------------------------------------------------------
  -- Then we'll draw a button that quits the app when hit.
  ------------------------------------------------------------------------------
  btnState <- button "Quit" [move 100 250] def
  let evClicked = fmapMaybe (guard . (== ButtonStateClicked)) $ updated btnState
  performEvent_ $ liftIO exitSuccess <$ evClicked

  btnA <- button "Button A" [move 50 5] def
  let evClickedA = fmapMaybe (guard . (== ButtonStateClicked)) $ updated btnA
  performEvent_ $ liftIO (putStrLn "Clicked A") <$ evClickedA

  btnB <- button "Button B" [move 70 13] def
  let evClickedB = fmapMaybe (guard . (== ButtonStateClicked)) $ updated btnB
  performEvent_ $ liftIO (putStrLn "Clicked B") <$ evClickedB

  iconBtn <- iconButton faAnchor [] def
  putDebugLnE (buttonClickedEvent iconBtn) $ const "Clicked the anchor."
  ------------------------------------------------------------------------------
  -- Then we'll draw a text field that says "Hello" and follows the mouse.
  ------------------------------------------------------------------------------
  evMotion          <- getMouseMotionEvent
  evButton          <- getMouseButtonEvent
  dPos              <- holdDyn 0 $ ffor evMotion $ \motion ->
                         let P v = mouseMotionEventPos motion
                         in fromIntegral <$> v
  let evTFTfrms = pure . moveV2 <$> updated dPos
  void $ flip holdView (return () <$ evButton) $
    textField 1 "Hello" [move 100 100] $ def & setTransformEvent .~ evTFTfrms
  ------------------------------------------------------------------------------
  -- Lastly we'll make sure that whenever a quit event comes in from the user
  -- we quit the app.
  ------------------------------------------------------------------------------
  getQuitEvent >>= performEvent_ . (liftIO exitSuccess <$)
  --------------------------------------------------------------------------------
  ---- Test directly writing widgets.
  --------------------------------------------------------------------------------
  V2V4Renderer v2v4 <- getV2V4
  k <- fresh
  r <- ((snd <$>) . liftIO) $ compilePicture v2v4 $
        setGeometry $ fan $ mapVertices (, 1) $ rectangle 0 100

  let square = Widget k [moveV2 100] [ShapeRectangle 0 100 100] r Nothing
  tellDyn $ constDyn $ pure square

  let leftTfrm = [move (-75) 75, multiply 1 1 0 1]
  tellDyn $ constDyn $ pure $ transformWidget square leftTfrm

  let rightTfrm = [move 75 75, multiply 1 0 1 1]
  tellDyn $ constDyn $ pure $ transformWidget square rightTfrm
  ------------------------------------------------------------------------------
  -- Test using 'varying' for animation.
  ------------------------------------------------------------------------------
  dToggledOn <- mdo
    dBtnState <- button "Stop animation" [move 200 10] $
      def & setTextEvent .~ evToggleText
    let evClickedToggle =
          fmapMaybe (guard . (== ButtonStateClicked)) $ updated dBtnState
    evNumClicks <- accum (+) (1 :: Int) $ 1 <$ evClickedToggle
    evToggledOn <- delayEventOneFrame $ odd <$> evNumClicks
    let evToggleText = ffor evToggledOn $ \isOn ->
          unwords [if isOn then "Stop" else "Start", "animation"]
    holdDyn True evToggledOn
  ev30FPS <- getFPSEvent 30
  evDelta <- (/1000) . fromIntegral <$$> performEventDelta ev30FPS
  let evGatedDelta = gate (current dToggledOn) evDelta
      myTween :: Var Float Float
      myTween = flip tweenStream 0 $ fix $ \loop -> do
       tween_ easeOutExpo (-50) 100 1
       tween_ easeOutExpo 100 (-50) 1
       loop
  dX <- varying myTween 0 evGatedDelta
  tellDyn $ ffor dX $ \x ->
    pure $ transformWidget square [move x 100, multiply 0 1 1 1, scale 0.5 0.5]
  ------------------------------------------------------------------------------
  -- Text input widget
  ------------------------------------------------------------------------------
  tiOutput <- textInput "Placeholder text..." [move 20 50] def
  putDebugLnE (updated $ tioState tiOutput)  $ ("textinput state: " ++) . show
  putDebugLnE (textInputEditedEvent tiOutput) $ ("edited: " ++) . show
  ------------------------------------------------------------------------------
  -- Test event sequencing
  ------------------------------------------------------------------------------
  let sequenceOfEvents n = do
        liftIO $ putStrLn $ "Round " ++ show n
        liftIO $ putStrLn "Step 1"
        liftE $ buttonClickedEvent <$> button "Next" [] def

        liftIO $ putStrLn "Step 2"
        liftE $ buttonClickedEvent <$> button "Next, again" [move 0 50] def

        liftIO $ putStrLn "Step 3"
        liftE getPostBuild

        liftIO $ putStrLn "Step 4"
        liftE $ buttonClickedEvent <$> button "Blah!" [move 0 100] def

        liftIO $ putStrLn "Rinse and repeat!"
        when (n < 2) $ sequenceOfEvents $ n + 1
  evDone <- runEventT $ sequenceOfEvents 0
  putDebugLnE evDone $ const "Done!"
  return ()


guest :: forall r t m. OdinWidget r t m => m ()
guest = do
  subguest
  layer (ShapeRectangle 0 300 300) 0.5 [move 150 20, rotate $ pi/8] def subguest

  let onClicked b = (b <$) . buttonClickedEvent
  setIsOnEvs <- alignInRow
    [ onClicked True  <$> button "Turn on checkbox"  [move 0 $ 200 - 32] def
    , onClicked False <$> button "Turn off checkbox" [move 10 $ 200 - 32] def
    , transform (constDyn [multiply 1 1 0 1]) $
        onClicked False <$> button "Turn off checkbox" [move 10 $ 200 - 32] def
    ]

  (dIsOn, dSt) <- checkbox False [move 0 200] $ leftmost setIsOnEvs
  putDebugLnE (updated dIsOn) show
  putDebugLnE (updated dSt) show


main :: IO ()
main = runOdin () $ runWidgets guest
