{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Odin.GUI.TextInput.Internal where

import           Gelatin
import           Odin.Core
import           Odin.GUI.Common
import           Odin.GUI.Button.Internal
import           Control.Lens hiding (to)
import           Control.Arrow (first)
import           Control.Monad (forM_, msum)
import           Data.List (intercalate)
import           Data.Monoid
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.IntMap as IM
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

data TextInputRndrs = TextInputRndrs { txtnRndrsUp       :: GUIRenderer
                                     , txtnRndrsOver     :: GUIRenderer
                                     , txtnRndrsDown     :: GUIRenderer
                                     , txtnRndrsEdit     :: GUIRenderer
                                     }

type WordMap = Map String (V2 Float, GLRenderer)

data TextInput m = TextInput { txtnSize    :: V2 Float
                             , txtnRndrs   :: TextInputRndrs
                             , txtnState   :: TextInputState
                             , txtnString  :: String
                             , txtnPainter :: Painter (TextInputData, TextInputState) m
                             }
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
delPressed :: UIState s m => m Bool
delPressed = do
  query <- use (ui . queryKey)
  let f k = query k Pressed False
  return $ any f [KeycodeBackspace, KeycodeDelete]

escOrEnterPressed :: UIState s m => m Bool
escOrEnterPressed = do
  query <- use (ui . queryKey)
  let f k = query k Pressed False
  return $ any f [KeycodeEscape, KeycodeReturn, KeycodeReturn2]

deleteString :: String -> String
deleteString t = if null t then [] else init t

getMyTextEvent :: UIState s m => String -> m (Maybe String)
getMyTextEvent str = do
  tstr <- use (ui . textEvent)
  if not $ null tstr
    then return $ Just $ str ++ tstr
    else delPressed >>= \case
      True  -> return $ Just $ deleteString str
      False -> return Nothing
--------------------------------------------------------------------------------
-- Alloc'ing and releasing TextInputs
--------------------------------------------------------------------------------
-- | Allocs a TextInput renderer for ONE TextInputState.
allocTextRndr :: (MonadIO m, Rezed s m, Fonts s m)
              => Painter (TextInputData, TextInputState) m
              -> TextInputState
              -> String
              -> m (GUIRenderer, V2 Float)
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
allocTextInput :: (MonadIO m, Rezed s m, Resources s m, Fonts s m)
               => Painter (TextInputData, TextInputState) m
               -> String
               -> m (Slot (TextInput m))
allocTextInput painter str = do
  (rs,sz) <- allocTextRndrs painter str
  s  <- allocSlot TextInput{ txtnSize    = sz
                           , txtnRndrs   = rs
                           , txtnState   = TextInputStateUp
                           , txtnString  = str
                           , txtnPainter = painter
                           }
  registerFree $ freeTextInput s
  return s

freeTextInput :: MonadIO m => Slot (TextInput x) -> m ()
freeTextInput s = fromSlot s f >>= io
  where f txt = do
          let TextInputRndrs{..} = txtnRndrs txt
              rs = [txtnRndrsUp,txtnRndrsOver,txtnRndrsDown,txtnRndrsEdit]
          forM_ rs fst

renderTextInput :: GUI s m
                => Slot (TextInput m) -> [RenderTransform]
                -> m (TextInputState, String)
renderTextInput s rs = do
  txt@TextInput{..} <- readSlot s
  let mv = affine2sModelview $ extractSpatial rs
      renderUp   = io $ (snd $ txtnRndrsUp   txtnRndrs) rs
      renderOver = io $ (snd $ txtnRndrsOver txtnRndrs) rs
      renderDown = io $ (snd $ txtnRndrsDown txtnRndrs) rs
      renderEdit = io $ (snd $ txtnRndrsEdit txtnRndrs) rs
      update st = do
        swapSlot s $ txt{txtnState=st}
        return (st, txtnString)
      downAndOver = (,) <$> (queryMouseButton ButtonLeft)
                        <*> (getMouseIsOverBox mv txtnSize)
      continueEditing str = do
        (r,sz) <- allocTextRndr txtnPainter TextInputStateEditing str
        let newRndrs = txtnRndrs{txtnRndrsEdit=r}
        io $ do -- dealloc the previous edit renderer
                fst $ txtnRndrsEdit txtnRndrs
                -- render the new edit
                snd r rs
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
                snd up rs
        swapSlot s txt{txtnSize    = sz
                      ,txtnRndrs   = newRndrs
                      ,txtnState   = TextInputStateUp
                      }
        return $ (TextInputStateEdited, str)
  case txtnState of
    TextInputStateUp -> getMouseIsOverBox mv txtnSize >>= \case
      True  -> do
        renderOver
        ui.systemCursor .= SDL_SYSTEM_CURSOR_HAND
        update TextInputStateOver
      False -> renderUp >> return (TextInputStateUp, txtnString)

    TextInputStateOver -> downAndOver >>= \case
    --(down,over)
      (False,True) -> do
        renderOver
        ui.systemCursor .= SDL_SYSTEM_CURSOR_HAND
        return (TextInputStateOver, txtnString)
      (True,True)  -> do
        renderDown
        ui.systemCursor .= SDL_SYSTEM_CURSOR_HAND
        update TextInputStateDown
      _            -> do
        renderUp
        update TextInputStateUp
    TextInputStateDown -> downAndOver >>= \case
    --(down,over)
      (False, True) -> do
        renderEdit
        ui.systemCursor .= SDL_SYSTEM_CURSOR_HAND
        update TextInputStateEditing
      (False, False) -> renderUp   >> update TextInputStateUp
      (True, ovr) -> do
        renderDown
        when ovr $ ui.systemCursor .= SDL_SYSTEM_CURSOR_HAND
        return (TextInputStateDown, txtnString)

    TextInputStateEditing -> escOrEnterPressed >>= \case
      True -> endEditing
      False -> getMyTextEvent txtnString >>= \case
        Nothing  -> downAndOver >>= \case
        --(down,over)
          (True,False) -> endEditing
          (_, ovr) -> do
            renderEdit
            when ovr $ ui.systemCursor .= SDL_SYSTEM_CURSOR_HAND
            return (TextInputStateEditing, txtnString)
        Just str -> continueEditing str

    TextInputStateEdited -> getMouseIsOverBox mv txtnSize >>= \case
      True  -> do
        renderOver
        ui.systemCursor .= SDL_SYSTEM_CURSOR_HAND
        update TextInputStateOver
      False -> renderUp >> update TextInputStateUp
--------------------------------------------------------------------------------
-- TextInput properties
--------------------------------------------------------------------------------
sizeOfTextInput :: MonadIO m => Slot (TextInput m) -> m (V2 Float)
sizeOfTextInput = flip fromSlot txtnSize
