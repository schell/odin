{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main where

import           Control.Lens                 hiding (to)
import           Control.Monad                (guard, void)
import           Data.Default                 (def)
import qualified Data.Vector.Unboxed          as V
import           Gelatin.GL
import           Reflex.SDL2                  hiding (fan)
import           System.Exit                  (exitSuccess)

import           Odin.Engine.New
import           Odin.Engine.New.UI.Button
import           Odin.Engine.New.UI.Configs
import           Odin.Engine.New.UI.Picture
import           Odin.Engine.New.UI.TextField


widgetLayer
  :: forall r t m a. OdinWidget r t m
  => (Dynamic t WidgetUserMotion -> DynamicWriterT t WidgetTree m a)
  -> m (a, Dynamic t WidgetUserMotion)
widgetLayer f = mdo
  (a, dWidgetTree) <- runDynamicWriterT $ f dMotion
  tellDyn dWidgetTree

  let dNodes = flattenWidgetTree [] <$> dWidgetTree
  dMousePos <- holdDyn (-1/0) =<< getMousePositionEvent
  dMotion   <- holdUniqDyn =<< foldDyn foldf UserNoMotion
    (updated $ zipDynWith widgetsHavePoint dNodes dMousePos)

  return (a, dMotion)
  where foldf True = \case
          UserNoMotion          -> UserMousedIntoWidget
          UserMousedIntoWidget  -> UserUsingWidget
          UserUsingWidget       -> UserUsingWidget
          UserMousedOutOfWidget -> UserMousedIntoWidget
        foldf False = \case
          UserNoMotion          -> UserNoMotion
          UserMousedIntoWidget  -> UserMousedOutOfWidget
          UserUsingWidget       -> UserMousedOutOfWidget
          UserMousedOutOfWidget -> UserNoMotion


guest :: forall r t m. (OdinWidget r t m) => m ()
guest = initialize12FPSEvent $ do
  widgetLayer $ \dUserMotion -> do
    putDebugLnE (updated dUserMotion) show
    k <- fresh
    tellDyn $ constDyn $ WidgetTreeLeaf k [] [ShapeRectangle 0 100 100] mempty
    widgetLayer $ \dUserMotionTop -> do
      putDebugLnE (updated dUserMotionTop) $ ("top " ++) . show
      k <- fresh
      tellDyn $ constDyn $ WidgetTreeLeaf k [] [ShapeRectangle 50 100 100] mempty
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


main :: IO ()
main = runOdin () $ mainLoop guest
