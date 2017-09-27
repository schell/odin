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
import           Control.Lens                 hiding (to)
import           Control.Monad                (guard, void)
import           Control.Monad.Fix            (fix)
import           Control.Monad.STM            (atomically)
import           Control.Varying              hiding (Event)
import           Data.Default                 (def)
import           Data.Monoid                  ((<>))
import qualified Data.Vector.Unboxed          as V
import           Data.Word                    (Word32)
import           Gelatin.GL
import           Reflex.SDL2                  hiding (fan)
import           System.Exit                  (exitSuccess)

import           Odin.Engine.New
import           Odin.Engine.New.UI.Animation
import           Odin.Engine.New.UI.Button
import           Odin.Engine.New.UI.Configs
import           Odin.Engine.New.UI.Picture
import           Odin.Engine.New.UI.TextInput
import           Odin.Engine.New.UI.TextField


guest :: forall r t m. OdinWidget r t m => m ()
guest = do
  ------------------------------------------------------------------------------
  -- First we'll draw a colorful background that always stays anchored at the
  -- top left corner, but also stretces to the boundaries of the window.
  ------------------------------------------------------------------------------
  let pic = setGeometry $ fan $ do
              to (     0, V4 1 1 0 1)
              to (V2 1 0, V4 0 1 1 1)
              to (     1, V4 1 0 1 1)
              to (V2 0 1, V4 1 1 1 1)
  evPB         <- getPostBuild
  evWindowSize <- getWindowSizeEvent
  let evTfrm = pure . scaleV2 . fmap fromIntegral <$> evWindowSize
  colorPicture $ def & setTransformEvent .~ evTfrm
                     & setPictureEvent   .~ (pic <$ evPB)
  ------------------------------------------------------------------------------
  -- Then we'll draw a button that quits the app when hit.
  ------------------------------------------------------------------------------
  btnOut <- button $ def & setTransformEvent .~ ([move 100 250] <$ evPB)
                         & setTextEvent      .~ ("Quit"         <$ evPB)
  let evClicked = fmapMaybe (guard . (== ButtonStateClicked)) $ updated $
                    buttonOutputState btnOut
  performEvent_ $ liftIO exitSuccess <$ evClicked
  -- Test stuff
  btnA <- button $ def & setTransformEvent .~ ([move 5 5] <$ evPB)
                       & setTextEvent      .~ ("Button A"         <$ evPB)
  let evClickedA = fmapMaybe (guard . (== ButtonStateClicked)) $ updated $
                    buttonOutputState btnA
  performEvent_ $ liftIO (putStrLn "Clicked A") <$ evClickedA
  btnB <- button $ def & setTransformEvent .~ ([move 20 13] <$ evPB)
                       & setTextEvent      .~ ("Button B"         <$ evPB)
  let evClickedB = fmapMaybe (guard . (== ButtonStateClicked)) $ updated $
                    buttonOutputState btnB
  performEvent_ $ liftIO (putStrLn "Clicked B") <$ evClickedB
  ------------------------------------------------------------------------------
  -- Then we'll draw a text field that says "Hello" and follows the mouse.
  ------------------------------------------------------------------------------
  DefaultFont fdesc <- getDefaultFont
  evMotion          <- getMouseMotionEvent
  evButton          <- getMouseButtonEvent
  dPos              <- holdDyn 0 $ ffor evMotion $ \motion ->
                         let P v = mouseMotionEventPos motion
                         in fromIntegral <$> v
  let evTFTfrms = leftmost [ pure . moveV2 <$> updated dPos
                           , [move 100 100] <$ evPB
                           ]
  void $ flip holdView (return () <$ evButton) $
    void $ textField $ def & setTextEvent           .~ ("Hello"    <$ evPB)
                           & setColorEvent          .~ (V4 1 1 1 1 <$ evPB)
                           & setFontDescriptorEvent .~ (fdesc      <$ evPB)
                           & setTransformEvent      .~ evTFTfrms
  ------------------------------------------------------------------------------
  -- Lastly we'll make sure that whenever a quit event comes in from the user
  -- we quit the app.
  ------------------------------------------------------------------------------
  getQuitEvent >>= performEvent_ . (liftIO exitSuccess <$)
  ------------------------------------------------------------------------------
  -- Test some stuff
  ------------------------------------------------------------------------------
  V2V4Renderer v2v4 <- getV2V4
  k <- fresh
  r <- ((snd <$>) . liftIO) $ compilePicture v2v4 $
    setGeometry $ fan $ mapVertices (, 1) $ rectangle 0 100

  let square = [Widget k [moveV2 100] [ShapeRectangle 0 100 100] r Nothing]
  dSquare <- holdDyn [] (square <$ evPB)
  tellDyn dSquare

  dLeftTfrm <- holdDyn [] ([move (-75) 75, multiply 1 1 0 1] <$ evPB)
  tellDyn $ zipDynWith transformWidgets dSquare dLeftTfrm

  dRightTfrm <- holdDyn [] ([move 75 75, multiply 1 0 1 1] <$ evPB)
  tellDyn $ zipDynWith transformWidgets dSquare dRightTfrm

  dToggledOn <- mdo
    btnToggle <- button $ def & setTransformEvent .~ ([move 200 10] <$ evPB)
                              & setTextEvent      .~ evToggleText
    let evClickedToggle = fmapMaybe (guard . (== ButtonStateClicked)) $ updated $
                            buttonOutputState btnToggle
    evNumClicks <- accum (+) 0 $ 1 <$ leftmost [evClickedToggle, evPB]
    let evToggledOn  = odd <$> evNumClicks
        evToggleText = ffor evToggledOn $ \isOn ->
          unwords [if isOn then "Stop" else "Start", "animation"]
    holdDyn True evToggledOn

  ev30FPS <-
    getRecurringTimerEventWithEventCode (fromIntegral $ fromEnum ORW30FPSEvent)
                                        $ floor (1000 / 30 :: Double)
  evDelta <- (/1000) . fromIntegral <$$> performEventDelta ev30FPS
  let evGatedDelta = gate (current dToggledOn) evDelta
      myTween :: Var Float Float
      myTween = flip tweenStream 0 $ fix $ \loop -> do
       tween_ easeOutExpo (-50) 100 1
       tween_ easeOutExpo 100 (-50) 1
       loop
  dX <- anime myTween $ def & deltaSecondsEvent .~ evGatedDelta
  tellDyn $ zipDynWith transformWidgets dSquare $ ffor dX $ \x ->
    [move x 100, multiply 0 1 1 1, scale 0.5 0.5]
  ------------------------------------------------------------------------------
  -- Text input widget
  ------------------------------------------------------------------------------
  evInputPos <-
    (\(V2 _ y) -> fromIntegral <$> V2 10 (y - 32)) <$$> getWindowSizeEvent
  tiOutput <- textInput "initial text" $
    def & setTransformEvent .~ (pure . moveV2 <$> evInputPos)
  putDebugLnE (updated $ tioState tiOutput)  $ ("textinput state: " ++) . show
  putDebugLnE (textInputEditedEvent tiOutput) $ ("edited: " ++) . show


main :: IO ()
main = runOdin () $ runWidgets guest
