{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Odin.GUI.TextInput.Internal where

import           Gelatin.SDL2
import           Gelatin.FreeType2
import           Odin.Core
import           Odin.GUI.Button.Internal
import           SDL
import qualified SDL.Raw.Types as Raw
import           Data.List (intercalate)
import           Data.Monoid
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import           Control.Lens hiding (to)
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data TextInputState = TextInputStateUp
                    | TextInputStateOver
                    | TextInputStateDown
                    | TextInputStateEditing
                    | TextInputStateEdited
                    deriving (Show, Eq)

data TextInputForegroundData = TextInputForegroundData
  { txtnDataAtlas     :: Atlas
  , txtnDataString    :: String
  }

data TextInputBackgroundData = TextInputBackgroundData
  { txtnDataSize :: V2 Float }

type TextInputStateRndr = (GLRenderer, WordMap)

renderTISR :: TextInputStateRndr -> PictureTransform -> IO ()
renderTISR isr t = snd (fst isr) t

deallocTISR :: TextInputStateRndr -> IO ()
deallocTISR isr = do
  fst $ fst isr
  void $ unloadMissingWords (snd isr) ""

-- | Dealloc the TISR but do not dealloc its word map.
deallocTISRBG :: TextInputStateRndr -> IO ()
deallocTISRBG = fst . fst

data TextInputRndrs = TextInputRndrs { txtnRndrsUp       :: TextInputStateRndr
                                     , txtnRndrsOver     :: TextInputStateRndr
                                     , txtnRndrsDown     :: TextInputStateRndr
                                     , txtnRndrsEdit     :: TextInputStateRndr
                                     }

type WordMap = Map String (V2 Float, GLRenderer)

data TextInput m = TextInput { txtnUid       :: Int
                             , txtnSize      :: V2 Float
                             , txtnRndrs     :: TextInputRndrs
                             , txtnState     :: TextInputState
                             , txtnString    :: String
                             , txtnFGPainter :: Painter (TextInputForegroundData, TextInputState) m
                             , txtnBGPainter :: Painter (TextInputBackgroundData, TextInputState) m
                             , txtnAtlas     :: Atlas
                             }
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
symIsDel :: Keysym -> Bool
symIsDel sym = key == KeycodeBackspace || key == KeycodeDelete
  where key = keysymKeycode sym

symIsEsc :: Keysym -> Bool
symIsEsc = (== KeycodeEscape) . keysymKeycode

symIsReturn :: Keysym -> Bool
symIsReturn sym = key == KeycodeReturn || key == KeycodeReturn2
    where key = keysymKeycode sym

deleteString :: String -> String
deleteString t = if null t then [] else init t

getKeyboardEvent :: Events s m => m (Maybe KeyboardEventData)
getKeyboardEvent = do
  evs <- use events
  return $ foldl f Nothing evs
  where f :: Maybe KeyboardEventData -> EventPayload -> Maybe KeyboardEventData
        f Nothing (KeyboardEvent ev) = Just ev
        f acc _ = acc

getEscEvent :: Events s m => m (Maybe KeyboardEventData)
getEscEvent = f <$> getKeyboardEvent
  where f (Just ev@(KeyboardEventData _ _ _ sym)) = if symIsEsc sym
                                                      then Just ev
                                                      else Nothing
        f _ = Nothing

getEnterEvent :: Events s m => m (Maybe KeyboardEventData)
getEnterEvent = f <$> getKeyboardEvent
  where f (Just ev@(KeyboardEventData _ _ _ sym)) = if symIsReturn sym
                                                      then Just ev
                                                      else Nothing
        f _ = Nothing

getEscOrEnter :: Events s m => m (Maybe KeyboardEventData)
getEscOrEnter = do
  mesc <- getEscEvent
  ment <- getEnterEvent
  return $ msum [mesc,ment]

getMyTextEvent :: Events s m => String -> m (Maybe String)
getMyTextEvent str = do
  evs <- use events
  let f Nothing (TextInputEvent (TextInputEventData _ t)) = Just t
      f acc _ = acc
      txtIn = foldl f Nothing evs
  return $ case txtIn of
    Just t -> Just $ str ++ T.unpack t
    Nothing ->
      let g :: Maybe Keysym -> EventPayload -> Maybe Keysym
          g Nothing (KeyboardEvent (KeyboardEventData _ Pressed _ sym)) = Just sym
          g acc _ = acc
          d :: Keysym -> String
          d sym = if symIsDel sym
                    then deleteString str
                    else str
      in d <$> foldl g Nothing evs
--------------------------------------------------------------------------------
-- WordMap stuff
--------------------------------------------------------------------------------
loadWords :: (MonadIO m, Rezed s m)
          =>  WordMap
          -> Painter (TextInputForegroundData, TextInputState) m
          -> TextInputState -> Atlas -> String
          -> m WordMap
loadWords wm0 painter st atlas str = foldM loadWord wm0 $ words str
  where loadWord wm word
          | Just _ <- M.lookup word wm = return wm
          | otherwise = do
            let dat = TextInputForegroundData atlas word
            sz <- runPainterSize painter (dat,st)
            r  <- compilePainter painter (dat,st)
            return $ M.insert word (sz,r) wm

unloadMissingWords :: MonadIO m => WordMap -> String -> m WordMap
unloadMissingWords wm str = do
  let ws = M.fromList $ zip (words str) [(0::Int)..]
      missing = M.difference wm ws
      retain  = M.difference wm missing
      dealoc  = (io . fst . snd) <$> missing
  sequence_ dealoc
  return retain
--------------------------------------------------------------------------------
-- Alloc'ing and releasing TextInputs
--------------------------------------------------------------------------------
allocFGRndr :: (MonadIO m, Rezed s m)
               => Painter (TextInputForegroundData, TextInputState) m
               -> TextInputState -> WordMap -> Atlas -> String
               -> m (RenderIO, V2 Float, WordMap)
allocFGRndr fgp st wm0 atlas str = do
  wm <- flip unloadMissingWords str =<< loadWords wm0 fgp st atlas str
  let glyphw  = glyphWidth $ atlasGlyphSize atlas
      spacew  = fromMaybe glyphw $ do
        metrics <- IM.lookup (fromEnum ' ') $ atlasMetrics atlas
        let V2 x _ = glyphAdvance metrics
        return $ fromIntegral x
      glyphh = glyphHeight $ atlasGlyphSize atlas
      renderWord :: PictureTransform -> Float -> String -> IO ()
      renderWord _ _ ""       = return ()
      renderWord t x (' ':cs) = renderWord t (x + spacew) cs
      renderWord t x cs       = do
        let word = takeWhile (/= ' ') cs
            rest = drop (length word) cs
        case M.lookup word wm of
          Nothing          -> renderWord t x rest
          Just (V2 w _, r) -> do
            snd r (t <> renderToPictureTransform (Spatial $ Translate $ V2 x 0))
            renderWord t (x + w) rest
      rr t = renderWord t 0 str
      measureWord x ""       = x
      measureWord x (' ':cs) = measureWord (x + spacew) cs
      measureWord x cs       =
        let word = takeWhile (/= ' ') cs
            rest = drop (length word) cs
            n    = case M.lookup word wm of
                     Nothing          -> x
                     Just (V2 w _, _) -> x + w
        in measureWord n rest
      ww = measureWord 0 str
  return (rr, V2 ww glyphh, wm)

allocFGRndr' :: (MonadIO m, Rezed s m)
               => Painter (TextInputForegroundData, TextInputState) m
               -> TextInputState -> WordMap -> Atlas -> String
               -> m (RenderIO, V2 Float, WordMap)
allocFGRndr' fgp st wm0 atlas str = do
  wm <- flip unloadMissingWords str =<< loadWords wm0 fgp st atlas str
  let ws = words str
      rs = map (`M.lookup` wm) ws
      glyphw  = glyphWidth $ atlasGlyphSize atlas
      spacew  = fromMaybe glyphw $ do
        metrics <- IM.lookup (fromEnum ' ') $ atlasMetrics atlas
        let V2 x _ = glyphAdvance metrics
        return $ fromIntegral x
      glyphh = glyphHeight $ atlasGlyphSize atlas
      rr t = when (not $ null rs) $ do
               x <- foldM (render t) 0 $ init rs
               void $ render t x $ last rs
      render _ x Nothing                = return x
      render t x (Just (V2 w _, (_,r))) = do
        let tt = renderToPictureTransform $ Spatial $ Translate $ V2 x 0
        r $ t <> tt
        return $ x + w + spacew
      measure x Nothing         = x
      measure x (Just (V2 w _,_)) = x + w + spacew
      tw = let tw0 = foldl measure 0 rs
           in if not $ null str
                then if last str == ' ' then tw0 - spacew else tw0 - 2*spacew
                else tw0
  return (rr, V2 tw glyphh, wm)

-- | Allocs a TextInput renderer for ONE TextInputState.
allocTextRndr :: (MonadIO m, Rezed s m)
              => Painter (TextInputForegroundData, TextInputState) m
              -> Painter (TextInputBackgroundData, TextInputState) m
              -> TextInputState
              -> WordMap -> Atlas -> String
              -> m (TextInputStateRndr, V2 Float)
allocTextRndr fg bg st wm0 atlas str = do
  (r,sz,wm) <- allocFGRndr fg st wm0 atlas str
  let dat = (TextInputBackgroundData sz, st)
  bgsz <- runPainterSize bg dat
  bgr  <- compilePainter bg dat
  let glrend :: GLRenderer
      glrend = bgr <> (return (),r)
      isr :: TextInputStateRndr
      isr = (glrend,wm)
  return (isr, bgsz)

allocTextRndrs :: (MonadIO m, Rezed s m)
               => Painter (TextInputForegroundData, TextInputState) m
               -> Painter (TextInputBackgroundData, TextInputState) m
               -> Atlas -> String
               -> m (TextInputRndrs, V2 Float)
allocTextRndrs fg bg atlas str = do
  (up  , sz) <- allocTextRndr fg bg TextInputStateUp      mempty atlas str
  (ovr ,  _) <- allocTextRndr fg bg TextInputStateOver    mempty atlas str
  (down,  _) <- allocTextRndr fg bg TextInputStateDown    mempty atlas str
  (edit,  _) <- allocTextRndr fg bg TextInputStateEditing mempty atlas str
  let rs = TextInputRndrs up ovr down edit
  return (rs, sz)

-- Allocs a new text input view.
allocTextInput :: (MonadIO m, Rezed s m, Fresh s m)
               => Atlas -> String
               -> Painter (TextInputForegroundData, TextInputState) m
               -> Painter (TextInputBackgroundData, TextInputState) m
               -> m (Slot (TextInput m))
allocTextInput atlas str fgpainter bgpainter = do
  (rs,sz) <- allocTextRndrs fgpainter bgpainter atlas str
  k  <- fresh
  allocSlot TextInput{ txtnUid       = k
                     , txtnSize      = sz
                     , txtnRndrs     = rs
                     , txtnState     = TextInputStateUp
                     , txtnString    = str
                     , txtnFGPainter = fgpainter
                     , txtnBGPainter = bgpainter
                     , txtnAtlas     = atlas
                     }

freeTextInput :: MonadIO m => Slot (TextInput x) -> m ()
freeTextInput s = fromSlot s f >>= io
  where f txt = do
          let TextInputRndrs{..} = txtnRndrs txt
              rs = [txtnRndrsUp,txtnRndrsOver,txtnRndrsDown,txtnRndrsEdit]
          forM_ rs deallocTISR

withTextInput :: (MonadIO m, Rezed s m, Fresh s m)
              => Atlas -> String
              -> Painter (TextInputForegroundData, TextInputState) m
              -> Painter (TextInputBackgroundData, TextInputState) m
              -> (Slot (TextInput m) -> m b) -> m b
withTextInput atlas str fgpainter bgpainter f = do
  txtn <- allocTextInput atlas str fgpainter bgpainter
  a    <- f txtn
  freeTextInput txtn
  return a

renderTextInput :: (MonadIO m, Rezed s m, Events s m)
                => Slot (TextInput m) -> [RenderTransform] -> m (TextInputState, String)
renderTextInput s rs = do
  txt@TextInput{..} <- readSlot s
  let t  = rendersToPictureTransform rs
      mv = ptfrmMV t
      renderUp   = io $ (renderTISR $ txtnRndrsUp   txtnRndrs) t
      renderOver = io $ (renderTISR $ txtnRndrsOver txtnRndrs) t
      renderDown = io $ (renderTISR $ txtnRndrsDown txtnRndrs) t
      renderEdit = io $ (renderTISR $ txtnRndrsEdit txtnRndrs) t
      update st = do
        swapSlot s $ txt{txtnState=st}
        return (st, txtnString)
      downAndOver = (,) <$> (($ ButtonLeft) <$> io getMouseButtons)
                        <*> (getMouseIsOverBox mv txtnSize)
      continueEditing str = do
        let ewm = snd $ txtnRndrsEdit txtnRndrs
        (r,sz) <- allocTextRndr txtnFGPainter txtnBGPainter
                                TextInputStateEditing ewm txtnAtlas str
        let newRndrs = txtnRndrs{txtnRndrsEdit=r}
        io $ do -- dealloc the previous edit renderer
                deallocTISRBG $ txtnRndrsEdit txtnRndrs
                -- render the new edit
                renderTISR r t
        swapSlot s txt{txtnSize     = sz
                      ,txtnRndrs    = newRndrs
                      ,txtnString   = str
                      ,txtnState    = TextInputStateEditing
                      }
        return $ (TextInputStateEditing, str)
      endEditing = do
        let str = txtnString
            uwm = snd $ txtnRndrsUp   txtnRndrs
            owm = snd $ txtnRndrsOver txtnRndrs
            dwm = snd $ txtnRndrsDown txtnRndrs
        -- realloc the other three rndrs
        (up,   sz) <- allocTextRndr txtnFGPainter txtnBGPainter
                                    TextInputStateUp uwm txtnAtlas
                                    str
        (ovr,   _) <- allocTextRndr txtnFGPainter txtnBGPainter
                                    TextInputStateOver owm txtnAtlas str
        (down,  _) <- allocTextRndr txtnFGPainter txtnBGPainter
                                    TextInputStateDown dwm txtnAtlas str
        let newRndrs = txtnRndrs{txtnRndrsUp    = up
                                ,txtnRndrsOver  = ovr
                                ,txtnRndrsDown  = down
                                }
        io $ do -- dealloc the previous 3 rndrs
                deallocTISRBG $ txtnRndrsUp   txtnRndrs
                deallocTISRBG $ txtnRndrsOver txtnRndrs
                deallocTISRBG $ txtnRndrsDown txtnRndrs
                -- render the new up
                renderTISR up t
        swapSlot s txt{txtnSize    = sz
                      ,txtnRndrs   = newRndrs
                      ,txtnState   = TextInputStateUp
                      }
        return $ (TextInputStateEdited, str)
  case txtnState of
    TextInputStateUp -> getMouseIsOverBox mv txtnSize >>= \case
      True  -> renderOver >> update TextInputStateOver
      False -> renderUp >> return (TextInputStateUp, txtnString)

    TextInputStateOver -> downAndOver >>= \case
    --(down,over)
      (False,True) -> renderOver >> return (TextInputStateOver, txtnString)
      (True,True)  -> renderDown >> update TextInputStateDown
      _            -> renderUp   >> update TextInputStateUp
    TextInputStateDown -> downAndOver >>= \case
    --(down,over)
      (False,True)  -> renderEdit >> update TextInputStateEditing
      (False,False) -> renderUp   >> update TextInputStateUp
      _             -> renderDown >> return (TextInputStateDown, txtnString)

    TextInputStateEditing -> getEscOrEnter >>= \case
      Just _  -> endEditing
      Nothing -> getMyTextEvent txtnString >>= \case
        Nothing  -> downAndOver >>= \case
        --(down,over)
          (True,False) -> endEditing
          _            -> renderEdit >> return (TextInputStateEditing, txtnString)
        Just str -> continueEditing str

    TextInputStateEdited -> getMouseIsOverBox mv txtnSize >>= \case
      True  -> renderOver >> update TextInputStateOver
      False -> renderUp >> update TextInputStateUp
--------------------------------------------------------------------------------
-- TextInput properties
--------------------------------------------------------------------------------
sizeOfTextInput :: MonadIO m => Slot (TextInput m) -> m (V2 Float)
sizeOfTextInput = flip fromSlot txtnSize
