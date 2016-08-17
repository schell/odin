{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Odin.GUI.TextInput.Internal where

import           Gelatin.SDL2
import           Gelatin.Fruity
import           Odin.Core
import           SDL
import qualified SDL.Raw.Types as Raw
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Lens hiding (to)

data TextInputState = TextInputStateUp
                    | TextInputStateOver
                    | TextInputStateDown
                    | TextInputStateEditing
                    | TextInputStateEdited String
                    deriving (Show, Eq)

data TextInputData = TextInputData { txtnDataFont      :: Font
                                   , txtnDataText      :: Text
                                   , txtnDataPointSize :: Float
                                   --, txtnInterval  :: Float
                                   }

data TextInputView t s r v = TextInputView
  { txtnData    :: TextInputData
  , txtnMailbox :: Mailbox TextInputState
  , txtnPainter :: Painter (TextInputData, TextInputState) t s r v
  , txtnCompiler:: Picture t s r v () -> System GLRenderer
  }

data TextInputRndrs = TextInputRndrs { txtRndrsUp   :: RenderIO
                                     , txtRndrsOver :: RenderIO
                                     , txtRndrsDown :: RenderIO
                                     }

symIsDel :: Keysym -> Bool
symIsDel sym = key == KeycodeBackspace || key == KeycodeDelete
  where key = keysymKeycode sym

symIsEsc :: Keysym -> Bool
symIsEsc = (== KeycodeEscape) . keysymKeycode

symIsReturn :: Keysym -> Bool
symIsReturn sym = key == KeycodeReturn || key == KeycodeReturn2
    where key = keysymKeycode sym

deleteText :: Text -> Text
deleteText t = if T.null t then T.empty else T.init t

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

getMyTextEvent :: Events s m => Text -> m (Maybe Text)
getMyTextEvent text = do
  evs <- use events
  let f Nothing (TextInputEvent (TextInputEventData _ t)) = Just t
      f acc _ = acc
      txtIn = foldl f Nothing evs
  return $ case txtIn of
    Just t -> Just $ text `T.append` t
    Nothing ->
      let g :: Maybe Keysym -> EventPayload -> Maybe Keysym
          g Nothing (KeyboardEvent (KeyboardEventData _ Pressed _ sym)) = Just sym
          g acc _ = acc
          d :: Keysym -> Text
          d sym = if symIsDel sym
                    then deleteText text
                    else text
      in d <$> foldl g Nothing evs

allocTextRndrs :: (Reads Rez m, DoesIO m)
               => TextInputView t s r v -> m (IO(), TextInputRndrs)
allocTextRndrs TextInputView{..} = do
  up   <- txtnCompiler $ txtnPainter (txtnData, TextInputStateUp)
  ovr  <- txtnCompiler $ txtnPainter (txtnData, TextInputStateOver)
  down <- txtnCompiler $ txtnPainter (txtnData, TextInputStateDown)
  let c = mapM_ fst [up,ovr,down]
      rs = TextInputRndrs (snd up) (snd ovr) (snd down)
  return (c, rs)

prepareTextInput :: TextInputView -> Entity -> System (V2 Float, TextInputRndrs)
prepareTextInput txt@TextInputView{..} k = do
  let sz = pictureSize' $ txtnPainter (txt, TextInputStateUp)
  (c,rs) <- allocTextRndrs txt
  k .# rndr (txtRndrsUp rs)
    #. dloc c
  return (sz, rs)

textInputEditing :: Mailbox TextInputState -> TextInput -> V2 Float -> Entity -> System Script
textInputEditing mb txt0 sz k = do
  let endEditing = do dealloc k
                      (sz1,rs) <- prepareTextInput txt0 k
                      send mb $ TextInputStateEdited $ T.unpack $ txtnText txt0
                      send mb TextInputStateUp
                      nextScript $ textInputUp mb txt0 sz1 rs k

  getEscOrEnter >>= \case
    Just _ -> endEditing
    Nothing -> getMyTextEvent (txtnText txt0) >>= \case
      Nothing -> do
        isDown <- ($ ButtonLeft) <$> io getMouseButtons
        isOver <- getMouseIsOverEntityWithSize k sz
        if isDown && not isOver
          then endEditing
          else nextScript $ textInputEditing mb txt0 sz k
      Just text -> do
        let txt1 = txt0{txtnText = text}
            pic  = textInputPainter (txt1, TextInputStateEditing)
            sz1  = pictureSize' pic
        (c,r) <- allocColorPicRenderer pic
        dealloc k
        k .# dloc c
          #. rndr r
        nextScript $ textInputEditing mb txt1 sz1 k

textInputDown :: Mailbox TextInputState -> TextInput -> V2 Float -> TextInputRndrs -> Entity -> System Script
textInputDown mb txt sz rs k = do
  isDown <- ($ ButtonLeft) <$> io getMouseButtons
  if not isDown
    then do isStillOver <- getMouseIsOverEntityWithSize k sz
            if isStillOver
              then do dealloc k
                      let pic = textInputPainter (txt, TextInputStateEditing)
                          sz1  = fst $ runPicture $ pic >> pictureSize
                      (c,r) <- allocColorPicRenderer pic
                      k .# dloc c
                        #. rndr r
                      io $ startTextInput $ Raw.Rect 0 0 100 100
                      send mb TextInputStateEditing
                      textInputEditing mb txt sz1 k
              else do rndrs.at k .= Just (txtRndrsUp rs)
                      send mb TextInputStateUp
                      nextScript $ textInputUp mb txt sz rs k
    else nextScript $ textInputDown mb txt sz rs k

textInputOver :: Mailbox TextInputState -> TextInput -> V2 Float -> TextInputRndrs -> Entity -> System Script
textInputOver mb txt sz rs k = do
  isDown <- ($ ButtonLeft) <$> io getMouseButtons
  isOver <- getMouseIsOverEntityWithSize k sz
  if isOver
    then if isDown then do rndrs.at k .= Just (txtRndrsDown rs)
                           send mb TextInputStateDown
                           textInputDown mb txt sz rs k
                   else nextScript $ textInputOver mb txt sz rs k
    else do rndrs.at k .= Just (txtRndrsUp rs)
            send mb TextInputStateUp
            textInputUp mb txt sz rs k

textInputUp :: TextInputView t (V2 Float) v r -> V2 Float -> TextInputRndrs -> Entity -> System Script
textInputUp mb txt sz rs k = do
  isOver <- getMouseIsOverEntityWithSize k sz
  if isOver then do rndrs.at k .= Just (txtRndrsOver rs)
                    send mb TextInputStateOver
                    textInputOver mb txt sz rs k
            else nextScript $ textInputUp mb txt sz rs k

textInputLifeCycle :: (Unbox v, Monoid (PictureData t (V2 Float) r v))
                   => TextInputView t (V2 Float) v r -> Entity -> Evented ()
textInputLifeCycle TextInputView{..} k = do



freshTextInput :: (Unbox v, Monoid (PictureData t (V2 Float) r v))
               => V2 Float -> TextInputView t (V2 Float) r v -> System Entity
freshTextInput p tview = do
  k        <- fresh ## pos p
  (sz, rs) <- prepareTextInput tview k
  scripts.at k .= Just [Script $ textInputUp tview sz rs k]
  return k
