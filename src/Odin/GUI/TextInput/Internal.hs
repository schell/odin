{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Odin.GUI.TextInput.Internal where

import           Gelatin.SDL2
import           Gelatin.FreeType2
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

data TextInputData = TextInputData { txtnDataAtlas     :: Atlas
                                   , txtnDataText      :: Text
                                   , txtnDataGlyphSize :: GlyphSize
                                   --, txtnInterval  :: Float
                                   }

data TextInputView = TextInputView
  { txtnData    :: TextInputData
  , txtnMailbox :: Mailbox TextInputState
  , txtnPainter :: Painter (TextInputData, TextInputState) System
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

allocTextRndrs :: TextInputView -> System (IO(), TextInputRndrs)
allocTextRndrs TextInputView{..} = do
  up   <- compilePainter txtnPainter (txtnData, TextInputStateUp)
  ovr  <- compilePainter txtnPainter (txtnData, TextInputStateOver)
  down <- compilePainter txtnPainter (txtnData, TextInputStateDown)
  let c = mapM_ fst [up,ovr,down]
      rs = TextInputRndrs (snd up) (snd ovr) (snd down)
  return (c, rs)

prepareTextInput :: TextInputView -> Entity -> System (V2 Float, TextInputRndrs)
prepareTextInput txt@TextInputView{..} k = do
  sz     <- runPainterSize txtnPainter (txtnData, TextInputStateUp)
  (c,rs) <- allocTextRndrs txt
  k .# rndr (txtRndrsUp rs)
    #. dloc c
  return (sz, rs)

textInputDown :: TextInputView -> V2 Float -> TextInputRndrs -> Entity
              -> System Script
textInputDown tview@TextInputView{..} sz rs k = do
  isDown <- ($ ButtonLeft) <$> io getMouseButtons
  if not isDown
    then do isStillOver <- getMouseIsOverEntityWithSize k sz
            if isStillOver
              then do dealloc k
                      sz1   <- runPainterSize txtnPainter
                                              (txtnData, TextInputStateEditing)
                      (c,r) <- compilePainter txtnPainter
                                               (txtnData, TextInputStateEditing)
                      k .# dloc c
                        #. rndr r
                      io $ startTextInput $ Raw.Rect 0 0 100 100
                      send txtnMailbox TextInputStateEditing
                      textInputEditing tview sz1 k
              else do rndrs.at k .= Just (txtRndrsUp rs)
                      send txtnMailbox TextInputStateUp
                      nextScript $ textInputUp tview sz rs k
    else nextScript $ textInputDown tview sz rs k

textInputOver :: TextInputView -> V2 Float -> TextInputRndrs -> Entity -> System Script
textInputOver tview@TextInputView{..} sz rs k = do
  isDown <- ($ ButtonLeft) <$> io getMouseButtons
  isOver <- getMouseIsOverEntityWithSize k sz
  if isOver
    then if isDown then do rndrs.at k .= Just (txtRndrsDown rs)
                           send txtnMailbox TextInputStateDown
                           textInputDown tview sz rs k
                   else nextScript $ textInputOver tview sz rs k
    else do rndrs.at k .= Just (txtRndrsUp rs)
            send txtnMailbox TextInputStateUp
            textInputUp tview sz rs k

textInputUp :: TextInputView -> V2 Float -> TextInputRndrs -> Entity -> System Script
textInputUp tview@TextInputView{..} sz rs k = do
  isOver <- getMouseIsOverEntityWithSize k sz
  if isOver then do rndrs.at k .= Just (txtRndrsOver rs)
                    send txtnMailbox TextInputStateOver
                    textInputOver tview sz rs k
            else nextScript $ textInputUp tview sz rs k

textInputEditing :: TextInputView -> V2 Float -> Entity -> System Script
textInputEditing tview@TextInputView{..} sz k = do
  let TextInputData{..} = txtnData
      endEditing = do dealloc k
                      (sz1,rs) <- prepareTextInput tview k
                      send txtnMailbox $ TextInputStateEdited $ T.unpack txtnDataText
                      send txtnMailbox TextInputStateUp
                      nextScript $ textInputUp tview sz1 rs k

  getEscOrEnter >>= \case
    Just _ -> endEditing
    Nothing -> getMyTextEvent txtnDataText >>= \case
      Nothing -> do
        isDown <- ($ ButtonLeft) <$> io getMouseButtons
        isOver <- getMouseIsOverEntityWithSize k sz
        if isDown && not isOver
          then endEditing
          else nextScript $ textInputEditing tview sz k
      Just text -> do
        let dat = txtnData{txtnDataText = text}
        sz1   <- runPainterSize txtnPainter (dat, TextInputStateEditing)
        (c,r) <- compilePainter txtnPainter (dat, TextInputStateEditing)
        dealloc k
        k .# dloc c
          #. rndr r
        nextScript $ textInputEditing tview{txtnData=dat} sz1 k

editingCycle :: TextInputView -> V2 Float -> Entity -> Evented ()
editingCycle tview@TextInputView{..} sz k = do
  let TextInputData{..} = txtnData
  e <- waitUntilAny [ (EditingEnd <$) <$> getEscOrEnter
                    , (Editing <$>) <$> getMyTextEvent txtnDataText
                    , do isDown <- ($ ButtonLeft) <$> io getMouseButtons
                         isOver <- getMouseIsOverEntityWithSize k sz
                         return $ if isDown && not isOver
                           then Just EditingEnd
                           else Nothing
                    ]
  io $ putStrLn $ "Text event:" ++ show e
  lift $ dealloc k
  case e of
    EditingEnd -> do
      lift $ do
        send txtnMailbox $ TextInputStateEdited $ T.unpack txtnDataText
        send txtnMailbox TextInputStateUp
      textInputLifeCycle tview{txtnData=txtnData} k
    Editing newText -> do
      let newDat = txtnData{txtnDataText = newText}
      sz2 <- lift $ do
        sz2   <- runPainterSize txtnPainter (newDat, TextInputStateEditing)
        (c,r) <- compilePainter txtnPainter (newDat, TextInputStateEditing)
        k .# dloc c
          #. rndr r
        return sz2
      next $ editingCycle tview{txtnData = newDat} sz2 k

downCycle :: TextInputView -> TextInputRndrs -> V2 Float -> Entity -> Evented ()
downCycle tview@TextInputView{..} rs sz k = do
  waitUntil $ (not . ($ ButtonLeft)) <$> io getMouseButtons
  (lift $ getMouseIsOverEntityWithSize k sz) >>= \case
    True -> do
      sz1 <- lift $ do
        dealloc k
        sz1   <- runPainterSize txtnPainter (txtnData, TextInputStateEditing)
        (c,r) <- compilePainter txtnPainter (txtnData, TextInputStateEditing)
        k .# dloc c
          #. rndr r
        io $ startTextInput $ Raw.Rect 0 0 100 100
        send txtnMailbox TextInputStateEditing
        return sz1
      editingCycle tview sz1 k
    False -> do
      lift $ do
        rndrs.at k .= Just (txtRndrsUp rs)
        send txtnMailbox TextInputStateUp
      upCycle tview rs sz k

overCycle :: TextInputView -> TextInputRndrs -> V2 Float -> Entity -> Evented ()
overCycle tview@TextInputView{..} rs sz k = do
  e <- waitUntilEither (($ ButtonLeft) <$> io getMouseButtons)
                       (not <$> getMouseIsOverEntityWithSize k sz)
  case e of
    Left ()  -> do lift $ do rndrs.at k .= Just (txtRndrsDown rs)
                             send txtnMailbox TextInputStateDown
                   downCycle tview rs sz k
    Right () -> do lift $ do rndrs.at k .= Just (txtRndrsUp rs)
                             send txtnMailbox TextInputStateUp
                   upCycle tview rs sz k

upCycle :: TextInputView -> TextInputRndrs -> V2 Float
        -> Entity -> Evented ()
upCycle tview@TextInputView{..} rs sz k = do
  -- Wait until the mouse is over the input
  waitUntil $ getMouseIsOverEntityWithSize k sz
  -- Set the input renderer to 'over' and send the 'over' message through to
  -- the mailbox
  lift $ do rndrs.at k .= Just (txtRndrsOver rs)
            send txtnMailbox TextInputStateOver
  -- Run the 'over' cycle
  overCycle tview rs sz k

data EditingEvent = Editing Text
                  | EditingEnd
                  deriving (Show, Eq)

textInputLifeCycle :: TextInputView -> Entity -> Evented ()
textInputLifeCycle tview k = do
  (sz, rs) <- lift $ prepareTextInput tview k
  upCycle tview rs sz k
--------------------------------------------------------------------------------
-- Alloc'ing Views and Fresh TextInput Entities
--------------------------------------------------------------------------------
-- Allocs a new text input view.
allocTextInputView :: Atlas -> String -> GlyphSize
                   -> Painter (TextInputData, TextInputState) System
                   -> (TextInputState -> System ())
                   -> System TextInputView
allocTextInputView font str px painter onRecv = do
  mb <- mailbox
  recv mb onRecv
  return $ TextInputView (TextInputData font (T.pack str) px) mb painter

freshTextInput :: V2 Float -> TextInputView -> System Entity
freshTextInput p tview = do
  k        <- fresh ## pos p
  (sz, rs) <- prepareTextInput tview k
  scripts.at k .= Just [Script $ textInputUp tview sz rs k]
  --scripts.at k .= Just [Script $ runEventedScript $ textInputLifeCycle tview k]
  return k
