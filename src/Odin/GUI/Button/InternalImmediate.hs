{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.GUI.Button.InternalImmediate where

import Control.Monad.Evented
import Gelatin.FreeType2
import Gelatin
import Gelatin.SDL2
import Control.Lens
import Linear.Affine (Point(..))
import SDL
import SDL.Input.Mouse (MouseButton(..)
                       ,getMouseButtons
                       )
import Odin.Core
--------------------------------------------------------------------------------
-- Button Types
--------------------------------------------------------------------------------
data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown
                 | ButtonStateClicked
                 deriving (Show, Eq, Ord, Enum, Bounded)

data ButtonData = ButtonData
  { btnDataAtlas     :: Atlas
  , btnDataStr       :: String
  }

data ButtonRndrs = ButtonRndrs
  { btnRndrsUp   :: RenderIO
  , btnRndrsOver :: RenderIO
  , btnRndrsDown :: RenderIO
  }

data Button = Button
  { btnUid     :: Int
  , btnSize    :: V2 Float
  , btnRndrs   :: ButtonRndrs
  , btnDealloc :: IO ()
  , btnState   :: ButtonState
  }
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
renderUp :: MonadIO m => Button -> PictureTransform -> m ()
renderUp Button{btnRndrs = ButtonRndrs{..}} = io . btnRndrsUp

renderOver :: MonadIO m => Button -> PictureTransform -> m ()
renderOver Button{btnRndrs = ButtonRndrs{..}} = io . btnRndrsOver

renderDown :: MonadIO m => Button -> PictureTransform -> m ()
renderDown Button{btnRndrs = ButtonRndrs{..}} = io . btnRndrsDown

getMouseIsOverBox :: MonadIO m => M44 Float -> V2 Float -> m Bool
getMouseIsOverBox mv sz = do
  P vi  <- io getAbsoluteMouseLocation
  let vf = fromIntegral <$> vi
      bb = over both (transformV2' mv) (0,sz)
  return $ pointInBounds vf bb

update :: MonadIO m => Slot Button -> ButtonState -> m ButtonState
update slot st = do
  modifySlot slot $ \btn -> btn{btnState = st}
  return st
--------------------------------------------------------------------------------
-- A Button's life cycle
--------------------------------------------------------------------------------
-- Allocs a new button.
allocButton :: Atlas -> String -> Painter (ButtonData, ButtonState) Update
            -> Update (Slot Button)
allocButton atlas str painter = do
  -- Alloc all our renderers up front
  let dat = ButtonData atlas str
  rz   <- lift $ use rez
  sz   <- runPainterSize painter (dat, ButtonStateUp)
  up   <- compilePainterZ rz painter (dat, ButtonStateUp)
  ovr  <- compilePainterZ rz painter (dat, ButtonStateOver)
  down <- compilePainterZ rz painter (dat, ButtonStateDown)
  let d  = mapM_ fst [up,ovr,down]
      rs = ButtonRndrs (snd up) (snd ovr) (snd down)
  lift $ do
    k <- fresh
    allocSlot $ Button k sz rs d ButtonStateUp

renderButton :: MonadIO m => Slot Button -> [RenderTransform] -> m ButtonState
renderButton s rs = do
  let t  = rendersToPictureTransform rs
      mv = ptfrmMV t
  btn@Button{..} <- checkSlot s
  case btnState of
    ButtonStateOver -> do
      isOver <- getMouseIsOverBox mv btnSize
      if isOver
        then do leftMouseIsDown <- ($ ButtonLeft) <$> io getMouseButtons
                if leftMouseIsDown
                  then do renderDown btn t
                          update s ButtonStateDown
                  else do renderOver btn t
                          return ButtonStateOver
        else do renderUp btn t
                update s ButtonStateUp
    ButtonStateDown -> do
      leftMouseIsUp <- (not . ($ ButtonLeft)) <$> io getMouseButtons
      if leftMouseIsUp
        then do renderUp btn t
                isOver <- getMouseIsOverBox mv btnSize
                update s $ if isOver then ButtonStateClicked else ButtonStateUp
        else do renderDown btn t
                return ButtonStateDown
    -- ButtonStateUp or ButtonStateClicked
    _   -> do
      isOver <- getMouseIsOverBox mv btnSize
      if isOver
        then do renderOver btn t
                update s ButtonStateOver
        else do renderUp btn t
                return ButtonStateUp

freeButton :: MonadIO m => Slot Button -> m ()
freeButton s = fromSlot s btnDealloc >>= io

withButton :: Atlas -> String -> Painter (ButtonData, ButtonState) Update
           -> (Slot Button -> Update b)
           -> Update b
withButton atlas str painter f = do
  btn <- allocButton atlas str painter
  a <- f btn
  freeButton btn
  return a
--------------------------------------------------------------------------------
-- Getting button properties
--------------------------------------------------------------------------------
stateOfButton :: MonadIO m => Slot Button -> m ButtonState
stateOfButton = flip fromSlot btnState

sizeOfButton :: MonadIO m => Slot Button -> m (V2 Float)
sizeOfButton = flip fromSlot btnSize
