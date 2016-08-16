{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Odin.Scripts.TextInput (
    TextInputState(..)
  , TextInput(..)
  , freshTextInput
  ) where

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

data TextInput = TextInput { txtnText      :: Text
                           , txtnFont      :: Font
                           , txtnPointSize :: Float
                           --, txtnInterval  :: Float
                           }

data TextInputRndrs = TextInputRndrs { txtRndrsUp   :: RenderIO
                                     , txtRndrsOver :: RenderIO
                                     , txtRndrsDown :: RenderIO
                                     }

textColor :: TextInputState -> V4 Float
textColor st = if st == TextInputStateEditing then canary else white

bgColor :: TextInputState -> V4 Float
bgColor TextInputStateDown = V4 0.3 0.3 0.3 1
bgColor TextInputStateEditing = V4 0.2 0.2 0.2 1
bgColor _ = V4 1 1 1 0

lnColor :: TextInputState -> V4 Float
lnColor TextInputStateUp = white `withAlpha` 0.4
lnColor TextInputStateOver = white `withAlpha` 0.8
lnColor TextInputStateDown = white
lnColor _ = white `withAlpha` 0.8

paintTextBackground :: V2 Float -> TextInputState -> ColorPicture ()
paintTextBackground sz@(V2 tw th) st = do
  let bgcolor = bgColor st
      lncolor = lnColor st
      padding = 4
      inc     = 1.5 * padding
  --withColor $ rectangle 0 (sz + V2 inc inc) $ const bgcolor
  setStroke [StrokeWidth 3, StrokeFeather 1]
  setGeometry $ geometry $ do
    add $ fan $ mapVertices (,bgcolor) $ vertices $ rectangle 0 (sz + V2 inc inc)
    add $ line $ vertices $ do
     to (0, lncolor)
     to (V2 (tw + inc) 0, lncolor)
     to (V2 (tw + inc) (th + inc), lncolor)
     to (V2 0 (th + inc), lncolor)
     to (0, lncolor)

paintTextInput :: TextInput -> TextInputState -> ColorPicture ()
paintTextInput txt st = do
  paintTextBackground size st
  foreground
  where textcolor= textColor st
        px = txtnPointSize txt
        txttxt = txtnText txt
        drawText = coloredString (txtnFont txt) 72 px (T.unpack txttxt) $ const textcolor
        leaderInc = if hasLeader then V2 inc 0 else 0
        endSpaces = T.length $ T.takeWhile (== ' ') $ T.reverse txttxt
        spaceInc = V2 (px/2) 0 ^* fromIntegral endSpaces
        V2 w h = sum [pictureSize' drawText, leaderInc, spaceInc]
        size@(V2 tw th) = V2 (max (px/2) w) (max px h)
        bar = setGeometry $ geometry $ add $ fan $
          mapVertices (,textcolor `withAlpha` 0.5) $ vertices $
            rectangle (V2 tw padding) (V2 (tw + 1.5) (th + padding))

        hasLeader = st == TextInputStateEditing
        padding = 4
        inc = 1.5 * padding
        foreground = do
          embed $ do
            move (V2 0 th + V2 padding padding)
            drawText
          when hasLeader $ embed bar

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

allocTextRndrs :: (Reads Rez m, DoesIO m) => TextInput -> m (IO(), TextInputRndrs)
allocTextRndrs txt = do
  up   <- allocColorPicRenderer $ paintTextInput txt TextInputStateUp
  ovr  <- allocColorPicRenderer $ paintTextInput txt TextInputStateOver
  down <- allocColorPicRenderer $ paintTextInput txt TextInputStateDown
  let c = mapM_ fst [up,ovr,down]
      rs = TextInputRndrs (snd up) (snd ovr) (snd down)
  return (c, rs)

prepareTextInput :: TextInput -> Entity -> System (V2 Float, TextInputRndrs)
prepareTextInput txt k = do
  let sz = pictureSize' $ paintTextInput txt TextInputStateUp
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
            pic  = paintTextInput txt1 TextInputStateEditing
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
                      let pic = paintTextInput txt TextInputStateEditing
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

textInputUp :: Mailbox TextInputState -> TextInput -> V2 Float -> TextInputRndrs -> Entity -> System Script
textInputUp mb txt sz rs k = do
  isOver <- getMouseIsOverEntityWithSize k sz
  if isOver then do rndrs.at k .= Just (txtRndrsOver rs)
                    send mb TextInputStateOver
                    textInputOver mb txt sz rs k
            else nextScript $ textInputUp mb txt sz rs k


freshTextInput :: TextInput -> Mailbox TextInputState -> System Entity
freshTextInput txt mb = do
  k        <- fresh ## tfrm mempty
  (sz, rs) <- prepareTextInput txt k
  scripts.at k .= Just [Script $ textInputUp mb txt sz rs k]
  return k
