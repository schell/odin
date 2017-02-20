{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Odin.Engine.GUI.TextInput.Internal where

import           Control.Monad                   (forM, forM_, when)
import           Data.Map                        (Map)
import           Gelatin
import           SDL                             hiding (get)
import           SDL.Raw.Enum
--------------------------------------------------------------------------------
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

newtype TextInputData = TextInputData { txtnDataStr :: String }

data TextInputRndrs = TextInputRndrs { txtnRndrsUp   :: Renderer2
                                     , txtnRndrsOver :: Renderer2
                                     , txtnRndrsDown :: Renderer2
                                     , txtnRndrsEdit :: Renderer2
                                     }

type WordMap = Map String (V2 Float, Renderer2)

data TextInput r = TextInput { txtnSize    :: V2 Float
                             , txtnRndrs   :: TextInputRndrs
                             , txtnState   :: TextInputState
                             , txtnString  :: String
                             , txtnPainter :: Painter (TextInputData, TextInputState) r
                             }
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
delPressed :: AltersUI r => Eff r Bool
delPressed = do
  presses <- forM keys $ \k -> queryKeycodeEvent k Pressed False
  return $ True `elem` presses
  where keys = [KeycodeBackspace, KeycodeDelete]

escOrEnterPressed :: AltersUI r => Eff r Bool
escOrEnterPressed = do
  presses <- forM keys $ \k -> queryKeycodeEvent k Pressed False
  return $ True `elem` presses
  where keys = [KeycodeEscape, KeycodeReturn, KeycodeReturn2]

deleteString :: String -> String
deleteString t = if null t then [] else init t

getMyTextEvent :: AltersUI r => String -> Eff r (Maybe String)
getMyTextEvent str = do
  tstr <- uiTextEvent <$> get
  if not $ null tstr
    then return $ Just $ str ++ tstr
    else delPressed >>= \case
      True  -> return $ Just $ deleteString str
      False -> return Nothing
--------------------------------------------------------------------------------
-- Alloc'ing and releasing TextInputs
--------------------------------------------------------------------------------
-- | Allocs a TextInput renderer for ONE TextInputState.
allocTextRndr :: Member IO r
              => Painter (TextInputData, TextInputState) r
              -> TextInputState
              -> String
              -> Eff r (Renderer2, V2 Float)
allocTextRndr painter st str = do
  let dat = TextInputData str
  Painting ((tl,br), r) <- unPainter painter (dat, st)
  return (r, br - tl)

allocTextRndrs :: Member IO r
               => Painter (TextInputData, TextInputState) r
               -> String
               -> Eff r (TextInputRndrs, V2 Float)
allocTextRndrs painter str = do
  (up  , sz) <- allocTextRndr painter TextInputStateUp      str
  (ovr ,  _) <- allocTextRndr painter TextInputStateOver    str
  (down,  _) <- allocTextRndr painter TextInputStateDown    str
  (edit,  _) <- allocTextRndr painter TextInputStateEditing str
  let rs = TextInputRndrs up ovr down edit
  return (rs, sz)

-- Allocs a new text input view.
slotTextInput :: (Member IO r, ReadsRenderers r, Member Allocates r)
               => Painter (TextInputData, TextInputState) r
               -> String
               -> Eff r (Slot (TextInput r))
slotTextInput painter str = do
  (rs, sz) <- allocTextRndrs painter str
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

renderTextInput
  :: (ReadsRenderers r, AltersUI r, Member IO r)
  => Slot (TextInput r)
  -> [RenderTransform2]
  -> Eff r (TextInputState, String)
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
        (r,sz) <- allocTextRndr txtnPainter TextInputStateEditing str
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
        reslot s txt{txtnSize    = sz
                    ,txtnRndrs   = newRndrs
                    ,txtnState   = TextInputStateUp
                    }
        return (TextInputStateEdited, str)
      enterOver = do
        renderOver
        setSystemCursor SDL_SYSTEM_CURSOR_HAND
        update TextInputStateOver
  case txtnState of
    TextInputStateUp -> getMouseIsOverBox mv txtnSize >>= \case
      True  -> enterOver
      False -> renderUp >> return (TextInputStateUp, txtnString)

    TextInputStateOver -> downAndOver >>= \case
    --(down,over)
      (False,True) -> do
        renderOver
        setSystemCursor SDL_SYSTEM_CURSOR_HAND
        return (TextInputStateOver, txtnString)
      (True,True)  -> do
        renderDown
        setSystemCursor  SDL_SYSTEM_CURSOR_HAND
        update TextInputStateDown
      _            -> do
        renderUp
        update TextInputStateUp
    TextInputStateDown -> downAndOver >>= \case
    --(down,over)
      (False, True) -> do
        renderEdit
        setSystemCursor  SDL_SYSTEM_CURSOR_HAND
        update TextInputStateEditing
      (False, False) -> renderUp   >> update TextInputStateUp
      (True, ovr) -> do
        renderDown
        when ovr $ setSystemCursor  SDL_SYSTEM_CURSOR_HAND
        return (TextInputStateDown, txtnString)

    TextInputStateEditing -> escOrEnterPressed >>= \case
      True  -> endEditing
      False -> getMyTextEvent txtnString >>= \case
        Nothing  -> downAndOver >>= \case
        --(down,over)
          (True,False) -> endEditing
          (_, ovr) -> do
            renderEdit
            when ovr $ setSystemCursor SDL_SYSTEM_CURSOR_HAND
            return (TextInputStateEditing, txtnString)
        Just str -> continueEditing str

    TextInputStateEdited -> getMouseIsOverBox mv txtnSize >>= \case
      True  -> enterOver
      False -> renderUp >> update TextInputStateUp
--------------------------------------------------------------------------------
-- TextInput properties
--------------------------------------------------------------------------------
sizeOfTextInput :: Member IO r => Slot (TextInput r) -> Eff r (V2 Float)
sizeOfTextInput = flip fromSlot txtnSize
