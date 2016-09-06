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
import           Control.Arrow (first)
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data TextInputState = TextInputStateUp
                    | TextInputStateOver
                    | TextInputStateDown
                    | TextInputStateEditing
                    | TextInputStateEdited
                    deriving (Show, Eq)

newtype TextInputData = TextInputData
  { txtnDataStr :: String }

data TextInputRndrs = TextInputRndrs { txtnRndrsUp       :: GLRenderer
                                     , txtnRndrsOver     :: GLRenderer
                                     , txtnRndrsDown     :: GLRenderer
                                     , txtnRndrsEdit     :: GLRenderer
                                     }

type WordMap = Map String (V2 Float, GLRenderer)

data TextInput m = TextInput { txtnUid     :: Int
                             , txtnSize    :: V2 Float
                             , txtnRndrs   :: TextInputRndrs
                             , txtnState   :: TextInputState
                             , txtnString  :: String
                             , txtnPainter :: Painter (TextInputData, TextInputState) m
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
-- Alloc'ing and releasing TextInputs
--------------------------------------------------------------------------------
-- | Allocs a TextInput renderer for ONE TextInputState.
allocTextRndr :: (MonadIO m, Rezed s m, Fonts s m)
              => Painter (TextInputData, TextInputState) m
              -> TextInputState
              -> String
              -> m (GLRenderer, V2 Float)
allocTextRndr painter st str = do
  let dat = TextInputData str
  Painting ((tl,br), r) <- unPainter painter (dat, st)
  return (r, br - tl)

allocTextRndrs :: (MonadIO m, Rezed s m, Fonts s m)
               => Painter (TextInputData, TextInputState) m
               -> String
               -> m (TextInputRndrs, V2 Float)
allocTextRndrs painter str = do
  (up  , sz) <- allocTextRndr painter TextInputStateUp      str
  (ovr ,  _) <- allocTextRndr painter TextInputStateOver    str
  (down,  _) <- allocTextRndr painter TextInputStateDown    str
  (edit,  _) <- allocTextRndr painter TextInputStateEditing str
  let rs = TextInputRndrs up ovr down edit
  return (rs, sz)

-- Allocs a new text input view.
allocTextInput :: (MonadIO m, Rezed s m, Fresh s m, Fonts s m)
               => Painter (TextInputData, TextInputState) m
               -> String
               -> m (Slot (TextInput m))
allocTextInput painter str = do
  (rs,sz) <- allocTextRndrs painter str
  k  <- fresh
  allocSlot TextInput{ txtnUid     = k
                     , txtnSize    = sz
                     , txtnRndrs   = rs
                     , txtnState   = TextInputStateUp
                     , txtnString  = str
                     , txtnPainter = painter
                     }

freeTextInput :: MonadIO m => Slot (TextInput x) -> m ()
freeTextInput s = fromSlot s f >>= io
  where f txt = do
          let TextInputRndrs{..} = txtnRndrs txt
              rs = [txtnRndrsUp,txtnRndrsOver,txtnRndrsDown,txtnRndrsEdit]
          forM_ rs fst

withTextInput :: (MonadIO m, Rezed s m, Fresh s m, Fonts s m)
              => Painter (TextInputData, TextInputState) m
              -> String
              -> (Slot (TextInput m) -> m b) -> m b
withTextInput painter str f = do
  txtn <- allocTextInput painter str
  a    <- f txtn
  freeTextInput txtn
  return a

renderTextInput :: (MonadIO m, Rezed s m, Fonts s m, Events s m)
                => Slot (TextInput m) -> [RenderTransform] -> m (TextInputState, String)
renderTextInput s rs = do
  txt@TextInput{..} <- readSlot s
  let t  = rendersToPictureTransform rs
      mv = ptfrmMV t
      renderUp   = io $ (snd $ txtnRndrsUp   txtnRndrs) t
      renderOver = io $ (snd $ txtnRndrsOver txtnRndrs) t
      renderDown = io $ (snd $ txtnRndrsDown txtnRndrs) t
      renderEdit = io $ (snd $ txtnRndrsEdit txtnRndrs) t
      update st = do
        swapSlot s $ txt{txtnState=st}
        return (st, txtnString)
      downAndOver = (,) <$> (($ ButtonLeft) <$> io getMouseButtons)
                        <*> (getMouseIsOverBox mv txtnSize)
      continueEditing str = do
        let ewm = snd $ txtnRndrsEdit txtnRndrs
        (r,sz) <- allocTextRndr txtnPainter TextInputStateEditing str
        let newRndrs = txtnRndrs{txtnRndrsEdit=r}
        io $ do -- dealloc the previous edit renderer
                fst $ txtnRndrsEdit txtnRndrs
                -- render the new edit
                snd r t
        swapSlot s txt{txtnSize     = sz
                      ,txtnRndrs    = newRndrs
                      ,txtnString   = str
                      ,txtnState    = TextInputStateEditing
                      }
        return $ (TextInputStateEditing, str)
      endEditing = do
        let str = txtnString
        -- realloc the other three rndrs
        (up,   sz) <- allocTextRndr txtnPainter TextInputStateUp   str
        (ovr,   _) <- allocTextRndr txtnPainter TextInputStateOver str
        (down,  _) <- allocTextRndr txtnPainter TextInputStateDown str
        let newRndrs = txtnRndrs{txtnRndrsUp    = up
                                ,txtnRndrsOver  = ovr
                                ,txtnRndrsDown  = down
                                }
        io $ do -- dealloc the previous 3 rndrs
                fst $ txtnRndrsUp   txtnRndrs
                fst $ txtnRndrsOver txtnRndrs
                fst $ txtnRndrsDown txtnRndrs
                -- render the new up
                snd up t
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
