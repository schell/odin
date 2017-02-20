{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE RecordWildCards  #-}
module Main where

import           Gelatin.SDL2 hiding (both)
import           SDL
import           Odin.Core
import           Odin.GUI
import           Control.Varying hiding (use)
import           Control.Lens (over, both)
import           Control.Monad (unless)
import           Control.Monad.Trans.State.Strict
import           Control.Concurrent.Async
import           Control.Concurrent (threadDelay)
import           Data.Maybe (listToMaybe)
import           Data.Hashable

import           Halive.Utils

checkpoint :: MonadIO m => String -> m a -> m a
checkpoint str = reacquire (fromIntegral $ hash str)

bgPicture :: Monad m => V2 Float -> V2 Float -> GLuint -> TexturePictureT m ()
bgPicture (V2 ww wh) (V2 tw th) tex = do
  setTextures [tex]
  setGeometry $ fan $
    mapVertices (\v@(V2 x y) -> (v, V2 (x/tw) (y/th))) $
      rectangle 0 $ V2 ww wh

renderBG :: (MonadIO m, CompileGraphics s m) => V2 Float -> GLuint
         -> Slot os (V2 Float) -> Slot os GUIRenderer
         -> m (V2 Float)
renderBG texsz tex lastWindowSize bg = do
  winsz1 <- unslot lastWindowSize
  winsz2 <- getWindowSize
  when (winsz1 /= winsz2) $ do
    void $ reslotTexturePicture bg $ bgPicture winsz2 texsz tex
    lastWindowSize `is` winsz2
  renderPicture bg []
  return winsz2

getDroppedFile :: (UIState s m) => m [FilePath]
getDroppedFile = use (ui . droppedFiles)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
--------------------------------------------------------------------------------
-- TileSet
--------------------------------------------------------------------------------
data TileSet os = TileSet { tilesetAll      :: Slot os GUIRenderer
                          -- ^ A renderer for all tiles at once
                          , tilesetSelector :: Slot os GUIRenderer
                          -- ^ A renderer for a selection background
                          , tilesetOutline  :: Slot os GUIRenderer
                          -- ^ A renderer for a selection outline
                          , tilesetSelected :: V2 Int
                          -- ^ The currently selected tile
                          , tilesetActive   :: V2 Int
                          -- ^ The currently active tile (active == hovered over)
                          , tilesetTileSize :: V2 Float
                          -- ^ The size (in pixels) of one tile
                          , tilesetWidth  :: Int
                          -- ^ The number of tile columns
                          , tilesetHeight :: Int
                          -- ^ The number of tile rows
                          }

tilesetTextureSize :: TileSet os -> V2 Float
tilesetTextureSize TileSet{..} =
  tilesetTileSize * (fromIntegral <$> V2 tilesetWidth tilesetHeight)

renderTileSet :: GUI s m => Slot os (TileSet os) -> [RenderTransform2] -> m ()
renderTileSet s rs = do
  tset@TileSet{..} <- unslot s
  (_,ts)          <- unslot tilesetAll
  (_,hov)         <- unslot tilesetSelector
  (_,out)         <- unslot tilesetOutline
  vi  <- getMousePosition
  let mxy = fromIntegral <$> vi
      mv  = affine2sModelview $ extractSpatial rs
      textureSize = tilesetTextureSize tset
      bb = over both (transformV2 mv) (0, textureSize)

  canBeActive <- getCanBeActive
  -- determine the hovered tile's x and y
  let rmv  = mxy - fst bb
      ndxsz = fromIntegral <$> V2 tilesetWidth tilesetHeight
      ndxs = (max 0 . fromIntegral . floor) <$> (ndxsz * rmv / (textureSize + ndxsz * 2))
      isOverTileset = pointInBounds mxy bb
      activendx = if canBeActive && isOverTileset
                    then ndxs
                    else fromIntegral <$> tilesetActive
      hovxy = activendx * tilesetTileSize + activendx * 2 - 2

  -- update the selected index by querying if the left mouse is down and over
  -- the tileset
  isDown <- queryMouseButton ButtonLeft
  let selndx = if canBeActive && isDown && isOverTileset
                 then activendx
                 else fromIntegral <$> tilesetSelected
      outxy = selndx * tilesetTileSize + selndx * 2 - 2

  io $ hov $ moveV2 hovxy:rs
  io $ ts rs
  io $ out $ moveV2 outxy:rs
  reslot s tset{tilesetActive = floor <$> activendx
               ,tilesetSelected = floor <$> selndx
               }

freeTileSet :: TileSet os -> m ()
freeTileSet TileSet{..} = do
  fst tilesetAll
  fst tilesetSelector

tilesetAllPic
  :: Monad m
  => V2 Float
  -> V2 Float
  -> GLuint
  -> TexturePictureT m (V2 Int)
tilesetAllPic (V2 imgw imgh) (V2 tilew tileh) tex = do
  let cols :: Int
      cols = floor $ imgw / tilew
      rows :: Int
      rows = floor $ imgh / tileh
      ndxs = [(fromIntegral x, fromIntegral y)
             | x <- [0..cols-1]
             , y <- [0..rows-1]
             ] :: [(Float,Float)]
      (w,h) = (tilew,tileh)
      nrm (V2 x y) = V2 (x/imgw) (y/imgh)
  setTextures [tex]
  let vs = flip concatMap ndxs $ \(x,y) ->
             let padding = V2 (x*2) (y*2)
                 tx = x*w
                 ty = y*h
                 tl = V2 tx         ty
                 tr = V2 (tx+w)     ty
                 br = V2 (tx+w) (ty+h)
                 bl = V2 tx     (ty+h)
             in [ (tl + padding, nrm tl)
                , (tr + padding, nrm tr)
                , (br + padding, nrm br)

                , (tl + padding, nrm tl)
                , (br + padding, nrm br)
                , (bl + padding, nrm bl)
                ]
  setGeometry $ triangles $ addVertexList vs
  return $ V2 cols rows

tilesetSelectorPic :: Monad m => V2 Float -> ColorPictureT m ()
tilesetSelectorPic tsz =
  setGeometry $ fan $ mapVertices (,black) $ rectangle 0 (tsz+4)

tilesetOutlinePic :: Monad m => V2 Float -> ColorPictureT m ()
tilesetOutlinePic (V2 x y) = do
  setStroke [StrokeWidth 3, StrokeFeather 1]
  setGeometry $ do
    let tl = 0
        tr = V2 (x+4) 0
        br = V2 (x+4) (y+4)
        bl = V2 0 (y+4)
        inc = 1
    line $ do to (tl+V2 inc inc, red)
              to (tr+V2 (-inc) inc, green)
              to (br+V2 (-inc) (-inc), blue)
              to (bl+V2 inc (-inc), orange)
              to (tl+V2 inc inc, red)
    line $ do to (tl, V4 1 1 0 1)
              to (tr, V4 0 1 1 1)
              to (br, V4 1 0 1 1)
              to (bl, orange)
              to (tl, V4 1 1 0 1)

slotTileSet
  :: (MonadIO m, CompileGraphics s m)
  => V2 Float
  -> V2 Float
  -> GLuint
  -> AllocatedT os m (Slotted os TileSet)
slotTileSet imgsz tsz tex = do
  (V2 w h, allgui) <- slotTexturePicture $ tilesetAllPic imgsz tsz tex
  (_, sel)         <- slotColorPicture $ tilesetSelectorPic tsz
  (_, out)         <- slotColorPicture $ tilesetOutlinePic tsz
  slot (TileSet allgui sel out 0 0 tsz w h) freeTileSet

reslotTileSet
  :: (MonadIO m, CompileGraphics s m)
  => Slotted os TileSet
  -> V2 Float
  -> V2 Float
  -> GLuint
  -> AllocatedT os m ()
reslotTileSet s imgsz tsz tex = do
  t@TileSet{..} <- unslot s
  V2 w h <- reslotTexturePicture tilesetAll $ tilesetAllPic imgsz tsz tex
  _      <- reslotColorPicture tilesetSelector $ tilesetSelectorPic tsz
  _      <- reslotColorPicture tilesetOutline $ tilesetOutlinePic tsz
  reslot s t{ tilesetTileSize = tsz
              , tilesetWidth    = w
              , tilesetHeight   = h
              }
--------------------------------------------------------------------------------
-- The MapMaker Task
--------------------------------------------------------------------------------
getDroppedImageFromUser
  :: MonadIO m
  => Slot os Text
  -> FontDescriptor
  -> V4 Float
  -> UpdateT os m (FilePath, V2 Float, GLuint)
getDroppedImageFromUser title font color = do
  renderText title [move 0 16]
  getDroppedFile >>= \case
    file:_ -> do
      io $ putStrLn $ "got a dropped file: " ++ show file
      ops <- use backOps
      io (backendOpAllocTexture ops file) >>= \case
        Nothing -> do
          reslotText title font black $ unwords [show file
                                                 ,"could not be loaded."
                                                 ,"Try another file."
                                                 ]
          next $ getDroppedImageFromUser title font color
        Just (imgtex, imgsz) -> return (file, fromIntegral <$> imgsz, imgtex)
    _ -> next $ getDroppedImageFromUser title font color

mapMaker :: MonadIO m => UpdateT os m ()
mapMaker = autoReleaseResources $ do
  -- Do a bunch of prep and slot our initial screen's GUI
  font <- getDefaultFontDescriptor
  title  <- slotText font black
    "Enter an amount of time (seconds) to demo waiting for forked processes"

  status <- slotStatusBar font black
  let renderStatus = do
        V2 w h <- getWindowSize
        renderStatusBar status [move (w-180) (h-4)]

  -----------------------------------------------------------------------------
  -- Demo concurrency with EventT
  -----------------------------------------------------------------------------
  -- First get the amount of time to delay our thread by
  nseconds <- autoReleaseResources $ do
    input <- slotTextInput textInputPainter "time"
    fix $ \getSeconds -> do
      renderStatus
      renderText title [move 0 16]
      renderTextInput input [move 4 20] >>= \case
        (TextInputStateEdited, str) -> case maybeRead str of
          Just n  -> return n
          Nothing -> next getSeconds
        _ -> next getSeconds

  -- show the time passing using an animation while we wait for the delayed
  -- thread to finish
  autoReleaseResources_ $ do
    -- slot a little progress bar
    (_,progress) <- slotColorPicture $ setGeometry $ fan $
      mapVertices (\v -> (v, yellow)) $ rectangle 0 $ V2 200 30
    -- slot an animation to keep track of our time progress
    anime <- slotAnime $ flip tweenStream 0 $ tween_ linear 0 nseconds nseconds
    let showProgress = do
          renderStatus
          tseconds <- stepAnime anime
          let percent = tseconds / nseconds
          renderPicture progress [scale percent 1]
          next showProgress
        delayedThread = do
          a <- io $ async $ threadDelay $ floor nseconds * 1000000
          fix $ \pollThread -> io (poll a) >>= \case
            Just (Right ()) -> return ()
            _ -> next pollThread
    withEither delayedThread showProgress >>= \case
      Left () -> io $ putStrLn "done waiting!"
      Right _ -> io $ putStrLn "this won't happen."

  -- Get a UI test image from the user
  reslotText title font black
    "Please drag and drop an image to use for the UI demo"
  (file, imgsz, imgtex) <- checkpoint "uitestimage" $
    getDroppedImageFromUser title font black

  io $ print (file, imgsz, imgtex)

  let stream :: Monad m => VarT m Float (Either (V2 Float) (V2 Float))
      stream = flip tweenStream (Left 0) $ fix $ \nxt -> do
        withTween_ easeOutExpo 700 400 1 $ \t -> Left  $ V2 t 400
        withTween_ easeOutExpo 400 300 1 $ \t -> Left  $ V2 400 t

        withTween_ easeOutExpo 0   300 1 $ \t -> Right $ V2 t   0
        withTween_ easeOutExpo 0   300 1 $ \t -> Right $ V2 300 t
        withTween_ easeOutExpo 300   0 1 $ \t -> Right $ V2 t 300
        withTween_ easeOutExpo 300   0 1 $ \t -> Right $ V2 0   t

        withTween_ easeOutExpo 400 700 1 $ \t -> Left  $ V2 t 300
        withTween_ easeOutExpo 300 400 1 $ \t -> Left  $ V2 700 t
        nxt

  nextBtn   <- slotButton buttonPainter "Next"
  (_, img)  <- slotTexturePicture $ do
    setTextures [imgtex]
    setGeometry $ fan $ mapVertices (\v -> (v, v/imgsz)) $ rectangle 0 imgsz
  ----------------------------------------------------------------------------
  -- Test out the Pane UI component
  ----------------------------------------------------------------------------
  autoReleaseResources_ $ do
    sizeTween <- slotAnime stream
    pane      <- slotPane (V2 800 400) (floor <$> imgsz) red

    fix $ \testPane -> do
      renderStatus
      stepAnime sizeTween >>= \case
        Left sizef    -> resizePane pane $ floor <$> sizef
        Right offsetf -> offsetPane pane $ floor <$> offsetf
      mpos <- renderPane pane [move 10 50] $ \offset -> do
        let V2 x y = fromIntegral <$> offset
        renderPicture img [move x y]
        getMousePosition
      renderButton nextBtn [move 10 10] >>= \case
        ButtonStateClicked -> return ()
        _ -> next testPane

    testBtn <- slotButton buttonPainter "Test"
    clicks  <- slot 0
    fix $ \testPane -> do
      renderStatus
      st <- renderPane pane [move 10 50] $ \offset -> do
        let moveOffset = moveV2 $ fromIntegral <$> offset
        renderPicture img [moveOffset]
        st <- renderButton testBtn [moveOffset, move 400 400]
        when (st == ButtonStateClicked) $ do
          n <- unslot clicks
          reslotButton testBtn buttonPainter $ "Clicked " ++ show (n + 1)
          clicks `is` (n + 1)
      renderButton nextBtn [move 10 10] >>= \case
        ButtonStateClicked -> return ()
        _ -> next testPane
  -----------------------------------------------------------------------------
  -- Test out the Panel UI compenent
  -----------------------------------------------------------------------------
  autoReleaseResources_ $ do
    panel <- slotPanel "A Panel!" 400 (floor <$> imgsz)

    fix $ \testPanel -> do
      renderStatus
      mpos <- renderPanel panel [move 10 50] $ \offset -> do
        let moveOffset = moveV2 $ fromIntegral <$> offset
        renderPicture img [moveOffset]
        getMousePosition
      renderButton nextBtn [move 10 10] >>= \case
        ButtonStateClicked -> return ()
        _ -> next testPanel
  -----------------------------------------------------------------------------
  -- Do the actual map maker!
  -----------------------------------------------------------------------------
  reslotText title font black
    "Please drag and drop an image to use as a tileset."
  (_,tsimgszf,tsimgtex) <- checkpoint "tilesetimage" $
    getDroppedImageFromUser title font black

  fix $ \toMappingScreen -> do
    -- Show the mapping screen
    let initialTW = 48
        initialTileSize = V2 initialTW initialTW
    autoReleaseResources_ $ do
      resetBtn    <- slotButton buttonPainter "Reset To Image Load"
      widthLabel  <- slotText font black "Tile Width:"
      widthInput  <- slotTextInput textInputPainter $ show initialTW
      heightLabel <- slotText font black "Tile Height:"
      heightInput <- slotTextInput textInputPainter $ show initialTW
      tileDimensions <- slot initialTileSize
      let renderTileDimensionInput rs = do
            renderText widthLabel       $ move 0 16 : rs
            delW <- renderTextInput widthInput  (move 0 18 : rs) >>= \case
              (TextInputStateEdited, str) -> case maybeRead str of
                Nothing -> return False
                Just w  -> do modifySlot os tileDimensions $ \(V2 _ h) -> V2 w h
                              return True
              _ -> return False
            renderText heightLabel      $ move 0 56 : rs
            delH <- renderTextInput heightInput (move 0 58 : rs) >>= \case
              (TextInputStateEdited, str) -> case maybeRead str of
                Nothing -> return False
                Just h -> do modifySlot os tileDimensions $ \(V2 w _) -> V2 w h
                             return True
              _ -> return False
            if delW || delH then Just <$> unslot tileDimensions
                           else return Nothing
      reslotText title font black $ unwords ["Using", show file]

      (_,tilesettsimg) <- slotTexturePicture $ do
        let V2 tsimgw tsimgh = tsimgszf
            sz :: V2 Float
            sz@(V2 szw szh) = if tsimgw >= tsimgh
                               then V2 100 (100/tsimgw * tsimgh)
                               else V2 (100/tsimgh * tsimgw) 100
        setTextures [tsimgtex]
        setGeometry $ fan $ mapVertices (\v@(V2 x y) -> (v, V2 (x/szw) (y/szh))) $
          rectangle 0 sz

      tiles <- slotTileSet tsimgszf initialTileSize tsimgtex
      panel <- slotPanel "Tileset" (V2 200 200) $ floor <$> tsimgszf

      fix $ \continue -> do
        renderStatus
        V2 _ h <- getWindowSize
        renderText title [move 0 $ h - 4]
        renderTileDimensionInput [] >>= \case
          Nothing -> return ()
          Just sz -> reslotTileSet tiles tsimgszf sz tsimgtex
        renderPicture tilesettsimg [move 100 0]
        (_,panelState) <- renderPanel panel [move 200 0] $ \offset -> do
          let offsetf = fromIntegral <$> offset
          renderTileSet tiles [moveV2 offsetf, move 2 2]
        renderButton resetBtn [move 0 100] >>= \case
          ButtonStateClicked -> return ()
          _ -> Control.Monad.unless (panelState == PanelStateShouldClose) $ next continue
    next toMappingScreen

runFrame :: MonadIO m => UpdateT os m a -> StateT Frame m a
runFrame f = do
  io $ glClearColor 0.5 0.5 0.5 1
  use backOps >>= io . backendOpClearWindow
  tickTime
  tickUIPrepare
  e <- runEventT f
  tickUIFinish
  use backOps >>= io . backendOpUpdateWindow
  case e of
    Left  a -> return a
    Right g -> runFrame g

main :: IO ()
main = do
  SDL2Backends v2v4 v2v2 <-
    reacquire 0 $ startupSDL2Backends 800 600 "Entity Sandbox" True
  t <- newTime
  let firstFrame = Frame { _frameTime    = t
                         , _frameNextK   = 0
                         , _frameV2v4c   = backendCompiler v2v4
                         , _frameV2v2c   = backendCompiler v2v2
                         , _frameBackOps = backendOps v2v4
                         , _frameFonts   = mempty
                         , _frameRsrcs   = []
                         , _frameUi      = emptyUi
                         }
  void $ runStateT (runFrame mapMaker) firstFrame
