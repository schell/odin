{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Gelatin.SDL2
import           SDL hiding (get, Event, time, windowSize)
import           Control.Varying
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.RWS.Strict
import           Data.Hashable
import qualified Data.IntMap.Strict as IM
import           Data.IntMap (IntMap)
import           Data.Char.FontAwesome
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))

import           App.Framework
import           App.Control.FRP
import           App.Control.Monad
import           App.GUI

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
--commitOrCancelKeyEvent :: Monad m => VarT m AppEvent (Event (Either () ()))
--commitOrCancelKeyEvent = eitherE cancl commit
--  where cancl = onKeyEvent 41 Released
--        commit = onKeyEvent 40 Released

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

--editTileSource :: TileSource -> AppSequence TileSource
--editTileSource ts = do
--  legible <- lift getLegibleFont
--  (mpic, ebtn) <- capture $ race (<>)
--    (pure (drawTileSource legible ts) `_untilEvent_` onKeyEvent 41 Released)
--    tileSourceEditBar
--
--  let pic = fromMaybe blank mpic
--  step pic
--
--  case ebtn of
--    Right btn -> editTileSourceWith ts btn >>= editTileSource
--    Left () -> return ts

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
      pure errText `_untilEvent_` anyKeyEvent
      getFirstTileSource
    Right (size, tx) -> do
      wsize0 <- lift $ asks rdWindowSize
      let wsize = fromIntegral <$> wsize0 :: V2 Float
          sz = fromIntegral <$> size :: V2 Float
          lp = LoadedPic tx size (floor <$> (wsize/2 - sz/2)) 1
      --editTileSource $ TileSource fp lp mempty
      return $ TileSource fp lp mempty

markupScreen :: AppSequence MarkupScreen
markupScreen = do
  ts <- getFirstTileSource
  infoStr "Got first TileSource"
  let ms = MarkupScreen $ IM.singleton 0 ts
  fnt <- lift getLegibleFont
  pure (drawMarkupScreen fnt ms) `untilEvent` anyKeyEvent
  return ms
  --(spinner <$> time <*> halfWindowSize) `_untilEvent_` anyKeydownEvent

data CancelCommitChoice = CancelChoice
                        | CommitChoice
                        deriving (Show, Eq, Enum, Bounded)

cancelCommitButtonBar :: V2 Float -> AppSequence CancelCommitChoice
cancelCommitButtonBar p = do
  icon <- lift getIconFont
  let cnclData = Button ButtonStateUp [faTimes] icon 16
      cncl   = button cnclData p
      V2 w _ = btnSize cnclData
      p1     = p + V2 (w + 1) 0
      chck   = button (Button ButtonStateUp [faCheck] icon 16) p1
  e <- race (<>) cncl chck
  either (const $ return CancelChoice) (const $ return CommitChoice) e

network :: AppSequence ()
network = do
  legi <- lift getLegibleFont
  let text = TextInput TextInputStateUp "a text input" legi 16 0
  race (<>) (textInput text 0)
           (cancelCommitButtonBar 100)
  network

main :: IO ()
main = runApp "TileSetMaker v0.0" network
