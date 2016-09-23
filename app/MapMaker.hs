{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE RecordWildCards  #-}
module Main where

import           Gelatin.SDL2 hiding (move, scale, rotate)
import           SDL
import           Gelatin.FreeType2
import           Odin.Core
import           Odin.GUI
import           Odin.GUI.Text.Internal
import           Control.Varying hiding (use)
import           Control.Lens (over, both, (&), (%~), (.=), (%=), (^.))
import           Control.Monad.Trans.State.Strict
import           Data.Maybe (listToMaybe)
import           Data.Hashable
import           Data.Char.FontAwesome
import           System.FilePath
import           System.Exit (exitFailure)
import           Paths_odin
import           Foreign.Marshal hiding (void)
import           Linear.Affine (Point(..))

import           Halive.Utils

checkpoint :: MonadIO m => String -> m a -> m a
checkpoint str = reacquire (fromIntegral $ hash str)

bgPicture :: Monad m => V2 Float -> V2 Float -> GLuint -> TexturePictureT m ()
bgPicture (V2 ww wh) (V2 tw th) tex = do
  setTextures [tex]
  setGeometry $ fan $
    mapVertices (\v@(V2 x y) -> (v, V2 (x/tw) (y/th))) $
      rectangle 0 $ V2 ww wh

renderBG :: (MonadIO m, Windowed s m, Rezed s m) => V2 Float -> GLuint
         -> Slot (V2 Float) -> Slot GUIRenderer
         -> m (V2 Float)
renderBG texsz tex lastWindowSize bg = do
  winsz1 <- unslot lastWindowSize
  winsz2 <- getWindowSize
  when (winsz1 /= winsz2) $ do
    void $ reallocTexturePicture bg $ bgPicture winsz2 texsz tex
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
data TileSet = TileSet { tilesetAll      :: Slot GUIRenderer
                       -- ^ A renderer for all tiles at once
                       , tilesetSelector :: Slot GUIRenderer
                       -- ^ A renderer for a selection background
                       , tilesetOutline  :: Slot GUIRenderer
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

tilesetTextureSize :: TileSet -> V2 Float
tilesetTextureSize TileSet{..} =
  tilesetTileSize * (fromIntegral <$> V2 tilesetWidth tilesetHeight)

renderTileSet :: GUI s m => Slot TileSet -> [RenderTransform] -> m ()
renderTileSet s rs = do
  tset@TileSet{..} <- readSlot s
  (_,ts)          <- readSlot tilesetAll
  (_,hov)         <- readSlot tilesetSelector
  (_,out)         <- readSlot tilesetOutline
  vi  <- getMousePosition
  let mxy = fromIntegral <$> vi
      mv  = affine2sModelview $ extractSpatial rs
      textureSize = tilesetTextureSize tset
      bb = over both (transformV2 mv) (0, textureSize)

  canBeActive <- getCanBeActive

  -- determine the hovered tile's x and y
  let rmv  = mxy - fst bb
      ndxsz = fromIntegral <$> (V2 tilesetWidth tilesetHeight)
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
  swapSlot s tset{tilesetActive = floor <$> activendx
                 ,tilesetSelected = floor <$> selndx
                 }

freeTileSet :: MonadIO m => Slot TileSet -> m ()
freeTileSet s = do
  TileSet{..} <- readSlot s
  fromSlotM tilesetAll $ io . fst
  fromSlotM tilesetSelector $ io . fst

tilesetAllPic :: Monad m
            => V2 Float -> V2 Float -> GLuint -> TexturePictureT m (V2 Int)
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
tilesetSelectorPic tsz = do
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

allocTileSet :: (MonadIO m, Resources s m, Rezed s m)
           => V2 Float -> V2 Float -> GLuint -> m (Slot TileSet)
allocTileSet imgsz tsz tex = do
  (V2 w h, allgui) <- allocTexturePicture $ tilesetAllPic imgsz tsz tex
  (_, sel)         <- allocColorPicture $ tilesetSelectorPic tsz
  (_, out)         <- allocColorPicture $ tilesetOutlinePic tsz
  s <- slot $ TileSet allgui sel out 0 0 tsz w h
  registerFree $ freeTileSet s
  return s

reallocTileSet :: (MonadIO m, Rezed s m)
             => Slot TileSet -> V2 Float -> V2 Float -> GLuint -> m ()
reallocTileSet s imgsz tsz tex = do
  t@TileSet{..} <- readSlot s
  V2 w h <- reallocTexturePicture tilesetAll $ tilesetAllPic imgsz tsz tex
  _      <- reallocColorPicture tilesetSelector $ tilesetSelectorPic tsz
  _      <- reallocColorPicture tilesetOutline $ tilesetOutlinePic tsz
  swapSlot s t{ tilesetTileSize = tsz
              , tilesetWidth    = w
              , tilesetHeight   = h
              }
--------------------------------------------------------------------------------
-- The MapMaker Task
--------------------------------------------------------------------------------
getDroppedImageFromUser :: MonadIO m
                        => Slot Text -> FontDescriptor -> V4 Float
                        -> UpdateT m (FilePath, V2 Float, GLuint)
getDroppedImageFromUser title font color = do
  renderText title [move 0 16]
  getDroppedFile >>= \case
    file:_ -> (io $ loadImage file) >>= \case
      Nothing -> do
        reallocText title font black $ unwords [show file
                                               ,"could not be loaded."
                                               ,"Try another file."
                                               ]
        next $ getDroppedImageFromUser title font color
      Just (imgsz, imgtex) -> return (file, fromIntegral <$> imgsz, imgtex)
    _ -> next $ getDroppedImageFromUser title font color

mapMaker :: MonadIO m => UpdateT m ()
mapMaker = autoReleaseResources $ do
  -- Do a bunch of prep and alloc our initial screen's GUI
  font <- getDefaultFontDescriptor
  title  <- allocText font black
    "Please drag and drop an image to use for the UI demo"

  -- Get a UI test image from the user
  (file, imgsz, imgtex) <- checkpoint "uitestimage" $
    getDroppedImageFromUser title font black

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

  nextBtn   <- allocButton buttonPainter "Next"
  (_, img)  <- allocTexturePicture $ do
    setTextures [imgtex]
    setGeometry $ fan $ mapVertices (\v -> (v, v/imgsz)) $ rectangle 0 imgsz
  ----------------------------------------------------------------------------
  -- Test out the Pane UI component
  ----------------------------------------------------------------------------
  autoReleaseResources_ $ do
    sizeTween <- allocAnime stream
    pane      <- allocPane (V2 800 400) (floor <$> imgsz) red

    fix $ \testPane -> do
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

    testBtn <- allocButton buttonPainter "Test"
    clicks  <- slot 0
    fix $ \testPane -> do
      st <- renderPane pane [move 10 50] $ \offset -> do
        let moveOffset = moveV2 $ fromIntegral <$> offset
        renderPicture img [moveOffset]
        st <- renderButton testBtn [moveOffset, move 400 400]
        when (st == ButtonStateClicked) $ do
          n <- unslot clicks
          reallocButton testBtn buttonPainter $ "Clicked " ++ (show $ n + 1)
          clicks `is` (n + 1)
      renderButton nextBtn [move 10 10] >>= \case
        ButtonStateClicked -> return ()
        _ -> next testPane

  -----------------------------------------------------------------------------
  -- Test out the Panel UI compenent
  -----------------------------------------------------------------------------
  autoReleaseResources_ $ do
    panel <- allocPanel "A Panel!" 400 (floor <$> imgsz)

    fix $ \testPanel -> do
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
  reallocText title font black
    "Please drag and drop an image to use as a tileset."
  (_,tsimgszf,tsimgtex) <- checkpoint "tilesetimage" $
    getDroppedImageFromUser title font black

  fix $ \toMappingScreen -> do
    -- Show the mapping screen
    let initialTW = 48
        initialTileSize = V2 initialTW initialTW
    autoReleaseResources_ $ do
      resetBtn    <- allocButton buttonPainter "Reset To Image Load"
      widthLabel  <- allocText font black "Tile Width:"
      widthInput  <- allocTextInput textInputPainter $ show initialTW
      heightLabel <- allocText font black "Tile Height:"
      heightInput <- allocTextInput textInputPainter $ show initialTW
      tileDimensions <- slot initialTileSize
      let renderTileDimensionInput rs = do
            renderText widthLabel       $ [move 0 16] ++ rs
            delW <- renderTextInput widthInput  ([move 0 18] ++ rs) >>= \case
              (TextInputStateEdited, str) -> case maybeRead str of
                Nothing -> return False
                Just w  -> do modifySlot tileDimensions $ \(V2 _ h) -> V2 w h
                              return True
              _ -> return False
            renderText heightLabel      $ [move 0 56] ++ rs
            delH <- renderTextInput heightInput ([move 0 58] ++ rs) >>= \case
              (TextInputStateEdited, str) -> case maybeRead str of
                Nothing -> return False
                Just h -> do modifySlot tileDimensions $ \(V2 w _) -> V2 w h
                             return True
              _ -> return False
            if delW || delH then Just <$> unslot tileDimensions
                           else return Nothing
      reallocText title font black $ unwords ["Using", show file]

      (_,tilesettsimg) <- allocTexturePicture $ do
        let V2 tsimgw tsimgh = tsimgszf
            sz :: V2 Float
            sz@(V2 szw szh) = if tsimgw >= tsimgh
                               then V2 100 (100/tsimgw * tsimgh)
                               else V2 (100/tsimgh * tsimgw) 100
        setTextures [tsimgtex]
        setGeometry $ fan $ mapVertices (\v@(V2 x y) -> (v, V2 (x/szw) (y/szh))) $
          rectangle 0 sz

      tiles <- allocTileSet tsimgszf initialTileSize tsimgtex
      panel <- allocPanel "Tileset" (V2 200 200) $ floor <$> tsimgszf

      fix $ \continue -> do
        V2 _ h <- getWindowSize
        renderText title [move 0 $ h - 4]
        renderTileDimensionInput [] >>= \case
          Nothing -> return ()
          Just sz -> reallocTileSet tiles tsimgszf sz tsimgtex
        renderPicture tilesettsimg [move 100 0]
        (_,panelState) <- renderPanel panel [move 200 0] $ \offset -> do
          let offsetf = fromIntegral <$> offset
          renderTileSet tiles [moveV2 offsetf, move 2 2]
        renderButton resetBtn [move 0 100] >>= \case
          ButtonStateClicked -> return ()
          _ -> if panelState == PanelStateShouldClose
                 then return ()
                 else next continue
    next toMappingScreen

runFrame :: MonadIO m => UpdateT m a -> StateT Frame m a
runFrame f = do
  io $ glClearColor 0.5 0.5 0.5 1
  use rez >>= io . clearFrame
  tickTime
  tickUIPrepare
  e <- runEventT f
  tickUIFinish
  use window >>= io . updateWindowSDL2
  case e of
    Left g   -> runFrame g
    Right a  -> return a

main :: IO ()
main = do
  (rz,win)  <- reacquire 0 $ startupSDL2Backend 800 600 "Entity Sandbox" True
  [fbw,fbh] <- withArray [0,0] $ \ptr -> do
    glGetIntegerv GL_MAX_VIEWPORT_DIMS ptr
    peekArray 2 ptr
  t         <- newTime
  let firstFrame = Frame { _frameTime   = t
                         , _frameNextK  = 0
                         , _frameWindow = win
                         , _frameRez    = rz
                         , _frameFonts  = mempty
                         , _frameRsrcs  = []
                         , _frameUi     = emptyUi
                         }
  void $ runStateT (runFrame mapMaker) firstFrame
