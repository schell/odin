{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE RecordWildCards  #-}
module Main where

import           Gelatin.SDL2 hiding (move)
import           SDL
import           Gelatin.FreeType2
import           Gelatin.Picture.Internal hiding (move)
import           Odin.GUI
import           Odin.GUI.Text.Internal
import           Odin.Core
import           Control.Lens (over, both)
import           Control.Monad.Trans.State.Strict
import           Data.Maybe (listToMaybe)
import           Data.Hashable
import           System.FilePath
import           System.Exit (exitFailure)
import           Paths_odin
import           Foreign.C.String
import           Linear.Affine (Point(..))

import           Halive.Utils

checkpoint :: MonadIO m => String -> m a -> m a
checkpoint str = reacquire (fromIntegral $ hash str)

getWindowSize :: (Windowed s m, MonadIO m) => m (V2 Float)
getWindowSize = do
  win <- use window
  (fmap fromIntegral) <$> io (SDL.get $ windowSize win)

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

getDroppedFile :: (MonadIO m, Events s m) => m [FilePath]
getDroppedFile = do
  let dd fs (DropEvent (DropEventData file)) = fs ++ [file]
      dd fs _ = fs
  (foldl dd [] <$> use events) >>= \case
    []    -> return []
    files -> mapM (io . peekCString) files

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

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

renderTileSet :: MonadIO m => Slot TileSet -> [RenderTransform] -> m ()
renderTileSet s rs = do
  tset@TileSet{..} <- readSlot s
  (_,ts)          <- readSlot tilesetAll
  (_,hov)         <- readSlot tilesetSelector
  (_,out)         <- readSlot tilesetOutline
  P vi  <- io getAbsoluteMouseLocation
  let mxy = fromIntegral <$> vi
      mv  = affine2sModelview $ extractSpatial rs
      textureSize = tilesetTextureSize tset
      bb = over both (transformV2 mv) (0, textureSize)

  -- determine the hovered tile's x and y
  let V2 tsw tsh = tilesetTileSize
      rmv  = mxy - fst bb
      ndxsz = fromIntegral <$> (V2 tilesetWidth tilesetHeight)
      ndxs = (max 0 . fromIntegral . floor) <$> (ndxsz * rmv / (textureSize + ndxsz * 2))
      isOverTileset = pointInBounds mxy bb
      activendx = if isOverTileset
                    then ndxs
                    else fromIntegral <$> tilesetActive
      hovxy = activendx * tilesetTileSize + activendx * 2 - 2

  -- update the selected index by querying if the left mouse is down and over
  -- the tileset
  isDown <- ($ ButtonLeft) <$> io getMouseButtons
  let selndx = if isDown && isOverTileset
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
tilesetOutlinePic tsz@(V2 x y) = do
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
  s <- allocSlot $ TileSet allgui sel out 0 0 tsz w h
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

mapMaker :: MonadIO m => UpdateT m ()
mapMaker = autoReleaseResources $ do
  -- Do a bunch of prep and alloc our initial screen's GUI
  font <- getDefaultFontDescriptor
  let path = "assets" </> "images" </> "checker-bg.png"
  checkerBgPath <- io $ getDataFileName path
  (winsz, lastWindowSize) <- do
    wsz <- getWindowSize
    (wsz,) <$> slot wsz
  (texsz, tex) <- io $ loadImage checkerBgPath >>= \case
    Nothing -> exitFailure
    Just (_,tx) -> return (16, tx)
  (_,bg) <- allocTexturePicture $ bgPicture winsz texsz tex
  title  <- allocText font black
    "Please drag and drop an image to use as a tileset"

  let renderBGAlias = renderBG texsz tex lastWindowSize bg

  -- Get a tileset image from the user
  (file, imgsz, imgtex) <- checkpoint "dropimage" $ fix $ \continue -> do
    (V2 _ wh) <- renderBGAlias
    renderText title [move 0 (wh - 2)]
    getDroppedFile >>= \case
      file:_ -> (io $ loadImage file) >>= \case
        Nothing -> do
          reallocText title font black $ unwords [show file
                                                 ,"could not be loaded."
                                                 ,"Try another file."
                                                 ]
          next continue
        Just (imgsz, imgtex) -> return (file, fromIntegral <$> imgsz, imgtex)
      _ -> next continue

  -- Show the mapping screen
  let initialTW = 48
      initialTileSize = V2 initialTW initialTW
  autoReleaseResources_ $ do
    testBtn     <- allocButton buttonPainter "Test Button"
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

    (_,tilesetimg) <- allocTexturePicture $ do
      let V2 imgw imgh = imgsz
          sz :: V2 Float
          sz@(V2 szw szh) = if imgw >= imgh
                             then V2 100 (100/imgw * imgh)
                             else V2 (100/imgh * imgw) 100
      setTextures [imgtex]
      setGeometry $ fan $ mapVertices (\v@(V2 x y) -> (v, V2 (x/szw) (y/szh))) $
        rectangle 0 sz

    tiles <- allocTileSet imgsz initialTileSize imgtex

    fix $ \continue -> do
      (V2 _ wh) <- renderBGAlias
      renderText title [move 0 (wh - 2)]
      renderTileDimensionInput [] >>= \case
        Nothing -> return ()
        Just sz -> reallocTileSet tiles imgsz sz imgtex
      renderPicture tilesetimg [move 100 0]
      renderTileSet tiles [move 200 0]
      renderButton testBtn [move 100 100]
      next continue

runFrame :: MonadIO m => UpdateT m a -> StateT Frame m a
runFrame f = do
  io $ glClearColor 0.5 0.5 0.5 1
  use rez >>= io . clearFrame
  tickTime
  tickEvents
  e <- runEventT f
  use window >>= io . updateWindowSDL2
  case e of
    Left g   -> runFrame g
    Right a  -> return a

main :: IO ()
main = do
  (rz,win)  <- reacquire 0 $ startupSDL2Backend 800 600 "Entity Sandbox" True
  t         <- newTime
  let firstFrame = Frame { _frameTime   = t
                         , _frameEvents = []
                         , _frameNextK  = 0
                         , _frameWindow = win
                         , _frameRez    = rz
                         , _frameFonts  = mempty
                         , _frameRsrcs  = []
                         }
  void $ runStateT (runFrame mapMaker) firstFrame