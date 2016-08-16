{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Gelatin.SDL2
import           SDL hiding (get, Event, time, windowSize)
import           Control.Varying
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Data.Char.FontAwesome
import           Data.Monoid ((<>))
import qualified Data.Vector.Unboxed as V
import qualified Data.Tiled as Tiled
import           Data.Tiled.Utils

import           App.Framework
import           App.Control.FRP
import           App.Control.Monad
import           App.GUI

data Layer = LayerPic Pic | LayerIO GLRenderer

crosshair :: V2 Float -> Pic
crosshair vec = move vec $ draw $ polylines [StrokeWidth 3, StrokeFeather 1] $
  do let len = 50
         c   = white `alpha` 0.75
     lineStart (0,c) $ lineTo (V2 len    0, c)
     lineStart (0,c) $ lineTo (V2 (-len) 0, c)
     lineStart (0,c) $ lineTo (V2 0    len, c)
     lineStart (0,c) $ lineTo (V2 0 (-len), c)

spinner :: Float -> V2 Float -> Pic
spinner t = rotate (pi * t) . crosshair

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

drawLoadedPic :: LoadedPic -> Pic
drawLoadedPic lp = do
  let V2 w h = fromIntegral <$> lpSize lp
      p = fromIntegral <$> lpPos lp
      s = V2 (lpScale lp) (lpScale lp)
      t = lpTex lp
  move p $ scale s $ withTexture t $
      fan (0,0) (V2 w 0,V2 1 0) (V2 w h, V2 1 1) $ V.singleton (V2 0 h, V2 0 1)

dragPic :: LoadedPic -> V2 Int -> AppSequence Pic LoadedPic
dragPic lp offset = do
  let mouseUp = mouseButtonEvent ButtonLeft Released
      lp1 = lp{ lpPos = lpPos lp + offset }
  (p,e) <- pure (drawLoadedPic lp1) `untilEvent` eitherE mouseUp mouseMotionEvent
  case e of
    Left _ -> transformPic lp{ lpPos = lpPos lp + offset }
    Right (_,v) -> step p >> dragPic lp (offset + v)

transformPic :: LoadedPic -> AppSequence Pic LoadedPic
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

iconButtonPic :: Font -> V4 Float -> V2 Float -> Float -> Char -> Picture a ()
iconButtonPic fnt color tl s ch = move (tl + V2 0 s) $ withLetters $
  filled fnt 72 s [ch] $ solid color

iconButtonDown :: V2 Float -> Float -> Char -> AppSequence Pic ()
iconButtonDown tl s ch = do
  icons <- lift getIconFont
  let pic = iconButtonPic icons gray tl s ch
      bounds = pictureBounds pic
  up <- pure pic `_untilEvent` mouseButtonEvent ButtonLeft Released
  step pic
  unless (pointInBounds (fromIntegral <$> up) bounds) $ iconButton tl s ch

iconButton :: V2 Float -> Float -> Char -> AppSequence Pic ()
iconButton tl s ch = do
  icons <- lift getIconFont
  let pic = iconButtonPic icons white tl s ch
      bounds = (tl,tl + V2 s s)

  down <- pure pic `_untilEvent` mouseButtonEvent ButtonLeft Pressed
  step pic

  if pointInBounds (fromIntegral <$> down) bounds
    then iconButtonDown tl s ch
    else iconButton tl s ch

data CancelCommitChoice = CancelChoice
                        | CommitChoice
                        deriving (Show, Eq, Enum, Bounded)

cancelCommitButtonBar :: V2 Float -> AppSequence Pic CancelCommitChoice
cancelCommitButtonBar p = do
  icon <- lift getIconFont
  let cnclData = Button ButtonStateUp [faTimes] icon 16
      cncl   = button cnclData p
      V2 w _ = btnSize cnclData
      p1     = p + V2 (w + 1) 0
      chck   = button (Button ButtonStateUp [faCheck] icon 16) p1
  e <- race (<>) cncl chck
  either (const $ return CancelChoice) (const $ return CommitChoice) e

network :: AppSequence [Layer] ()
network = do
  legi <- lift getLegibleFont
  let logo = withLetters . filled legi 128 64 "The Sandbox"
      logoWhite = move (V2 0 64) $ logo $ solid white
      logoRed   = move (V2 0 66) $ logo $ solid red
      logof rez = fst <$> compilePictureRenderer rez mempty logoRed
  r <- mapOutput (pure $ const []) $ loadLayer logof
  pure [LayerPic logoWhite, LayerIO r] `_untilEvent_` mouseButtonEvent ButtonLeft Released
  void $ mapOutput (pure $ const [LayerPic logoWhite]) $ unloadLayer r
  step [LayerPic logoWhite]
  pure [LayerPic logoWhite] `_untilEvent_` mouseButtonEvent ButtonLeft Released
  network

renderLayer :: (Pic, AppData [Layer]) -> Layer -> IO (Pic, AppData [Layer])
renderLayer acc (LayerIO r) = snd r mempty >> return acc
renderLayer (allPics, app) (LayerPic p) = do
  (r, cache) <- compilePictureRenderer (appRez app) (appCache app) p
  snd r mempty
  return (allPics >> p, app{ appCache = cache })

renderLayers :: Window -> AppData [Layer] -> [Layer] -> IO (AppData [Layer])
renderLayers window app layers = do
  clearFrame $ appRez app
  (pic, newApp) <- foldM renderLayer (blank, app) layers
  updateWindowSDL2 window
  newCache <- cleanPictureRendererCache (appCache newApp) pic
  return $ app{ appCache = newCache }

main :: IO ()
main = do
  tmap <- Tiled.loadMapFile "assets/oryx_ultimate_fantasy/uf_examples/uf_example_1.tmx"
  print $ unrelativizeImagePaths tmap
  runApp renderLayers (outputStream network []) "Odin's Litterbox"
