{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main where

import           Control.Lens                 hiding (to)
import           Control.Monad                (void, guard)
import           Data.Default                 (def)
import           Gelatin.GL
import           Reflex.SDL2                  hiding (fan)
import           System.Exit                  (exitSuccess)

import           Odin.Engine.New
import           Odin.Engine.New.UI.Configs
import           Odin.Engine.New.UI.Picture
import           Odin.Engine.New.UI.TextField
import           Odin.Engine.New.UI.Painters
import           Odin.Engine.New.UI.Button


guest :: forall r t m. (OdinLayered r t m) => m ()
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
