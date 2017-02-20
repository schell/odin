{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Odin.Engine.GUI.TextInput.Internal where

import           Control.Lens                    hiding (to)
import           Control.Monad                   (forM_)
import           Control.Monad.Trans             (lift)
import           Data.Map                        (Map)
import           Gelatin
import           Odin.Engine.Eff
import           Odin.Engine.GUI.Button.Internal
import           Odin.Engine.Slots
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

data TextInputRndrs = TextInputRndrs { txtnRndrsUp   :: Renderer2
                                     , txtnRndrsOver :: Renderer2
                                     , txtnRndrsDown :: Renderer2
                                     , txtnRndrsEdit :: Renderer2
                                     }

type WordMap = Map String (V2 Float, Renderer2)

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
allocTextRndr :: MonadIO m
              => Painter (TextInputData, TextInputState) m
              -> TextInputState
              -> String
              -> m (Renderer2, V2 Float)
allocTextRndr painter st str = do
  let dat = TextInputData str
  Painting ((tl,br), r) <- unPainter painter (dat, st)
  return (r, br - tl)

allocTextRndrs :: MonadIO m
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
slotTextInput :: (MonadIO m, CompileGraphics s m)
               => Painter (TextInputData, TextInputState) m
               -> String
               -> AllocatedT os m (Slot os (TextInput m))
slotTextInput painter str = do
  (rs,sz) <- lift $ allocTextRndrs painter str
  let t = TextInput{ txtnSize    = sz
                   , txtnRndrs   = rs
                   , txtnState   = TextInputStateUp
                   , txtnString  = str
                   , txtnPainter = painter
                   }
  slot t freeTextInput

freeTextInput :: TextInput x -> IO ()
freeTextInput txt = do
  let TextInputRndrs{..} = txtnRndrs txt
      rs = [txtnRndrsUp,txtnRndrsOver,txtnRndrsDown,txtnRndrsEdit]
  forM_ rs fst

renderTextInput :: GUI s m
                => Slot os (TextInput m) -> [RenderTransform2]
                -> AllocatedT os m (TextInputState, String)
renderTextInput s rs = do
  txt@TextInput{..} <- unslot s
  let mv = affine2sModelview $ extractSpatial rs
      renderUp   = io $ (snd $ txtnRndrsUp   txtnRndrs) rs
      renderOver = io $ (snd $ txtnRndrsOver txtnRndrs) rs
      renderDown = io $ (snd $ txtnRndrsDown txtnRndrs) rs
      renderEdit = io $ (snd $ txtnRndrsEdit txtnRndrs) rs
      update st = do
        reslot s $ txt{txtnState=st}
        return (st, txtnString)
      downAndOver = (,) <$> queryMouseButton ButtonLeft
                        <*> getMouseIsOverBox mv txtnSize
      continueEditing str = do
        (r,sz) <- lift $ allocTextRndr txtnPainter TextInputStateEditing str
        let newRndrs = txtnRndrs{txtnRndrsEdit=r}
        io $ do -- dealloc the previous edit renderer
                fst $ txtnRndrsEdit txtnRndrs
                -- render the new edit
                snd r rs
        reslot s txt{txtnSize     = sz
                    ,txtnRndrs    = newRndrs
                    ,txtnString   = str
                    ,txtnState    = TextInputStateEditing
                    }
        return (TextInputStateEditing, str)
      endEditing = do
        let str = txtnString
        -- realloc the other three rndrs
        (up,   sz) <- lift $ allocTextRndr txtnPainter TextInputStateUp   str
        (ovr,   _) <- lift $ allocTextRndr txtnPainter TextInputStateOver str
        (down,  _) <- lift $ allocTextRndr txtnPainter TextInputStateDown str
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
        reslot s txt{txtnSize    = sz
                    ,txtnRndrs   = newRndrs
                    ,txtnState   = TextInputStateUp
                    }
        return (TextInputStateEdited, str)
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
sizeOfTextInput :: MonadIO m => Slot os (TextInput m) -> AllocatedT os m (V2 Float)
sizeOfTextInput = flip fromSlot txtnSize
