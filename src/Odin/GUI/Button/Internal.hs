{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.GUI.Button.Internal where

import Gelatin.FreeType2
import Gelatin
import Gelatin.SDL2
import Control.Lens
import Linear.Affine (Point(..))
import SDL hiding (freeCursor)
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Enum as Raw
import qualified SDL.Raw.Types as Raw
import Odin.Core
--------------------------------------------------------------------------------
-- Button Types
--------------------------------------------------------------------------------
data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown
                 | ButtonStateClicked
                 deriving (Show, Eq, Ord, Enum, Bounded)

newtype ButtonData = ButtonData
  { btnDataStr  :: String }

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
getMouseIsOverBox :: MonadIO m => M44 Float -> V2 Float -> m Bool
getMouseIsOverBox mv sz = do
  P vi  <- io getAbsoluteMouseLocation
  let vf = fromIntegral <$> vi
      bb = over both (transformV2' mv) (0,sz)
  return $ pointInBounds vf bb
--------------------------------------------------------------------------------
-- A Button's life cycle
--------------------------------------------------------------------------------
-- Allocs a new button.
allocButton :: (MonadIO m, Fresh s m, Rezed s m, Fonts s m)
            => Painter (ButtonData, ButtonState) m -> String
            -> m (Slot Button)
allocButton painter str = do
  -- Alloc all our renderers up front
  let dat = ButtonData str
  k    <- fresh
  Painting (bounds, up  ) <- unPainter painter (dat, ButtonStateUp)
  Painting (     _, ovr ) <- unPainter painter (dat, ButtonStateOver)
  Painting (     _, down) <- unPainter painter (dat, ButtonStateDown)
  let d  = mapM_ fst [up,ovr,down]
      rs = ButtonRndrs (snd up) (snd ovr) (snd down)
  allocSlot $ Button k (uncurry (flip (-)) bounds) rs d ButtonStateUp

freeButton :: MonadIO m => Slot Button -> m ()
freeButton s = fromSlot s btnDealloc >>= io

withButton :: (MonadIO m, Fresh s m, Rezed s m, Fonts s m)
           => Painter (ButtonData, ButtonState) m -> String
           -> (Slot Button -> m b)
           -> m b
withButton painter str f = do
  btn <- allocButton painter str
  a <- f btn
  freeButton btn
  return a

switchCursor :: MonadIO m => Raw.SystemCursor -> m ()
switchCursor k = do
  acursor <- Raw.getCursor
  ncursor <- Raw.createSystemCursor k
  Raw.setCursor ncursor
  Raw.freeCursor acursor

renderButton :: MonadIO m => Slot Button -> [RenderTransform] -> m ButtonState
renderButton s rs = do
  btn@Button{..} <- readSlot s
  let t             = rendersToPictureTransform rs
      mv            = ptfrmMV t
      renderBtnUp   = io $ btnRndrsUp btnRndrs t
      renderBtnOver = io $ btnRndrsOver btnRndrs t
      renderBtnDown = io $ btnRndrsDown btnRndrs t
      updateBtn st  = swapSlot s btn{btnState = st} >> return st
  case btnState of
    ButtonStateOver -> do
      isOver <- getMouseIsOverBox mv btnSize
      if isOver
        then do leftMouseIsDown <- ($ ButtonLeft) <$> io getMouseButtons
                if leftMouseIsDown
                  then do renderBtnDown
                          updateBtn ButtonStateDown
                  else do renderBtnOver
                          return ButtonStateOver
        else do renderBtnUp
                updateBtn ButtonStateUp
    ButtonStateDown -> do
      leftMouseIsUp <- (not . ($ ButtonLeft)) <$> io getMouseButtons
      if leftMouseIsUp
        then do renderBtnUp
                isOver <- getMouseIsOverBox mv btnSize
                updateBtn $ if isOver then ButtonStateClicked else ButtonStateUp
        else do renderBtnDown
                return ButtonStateDown
    -- ButtonStateUp or ButtonStateClicked
    _   -> do
      isOver <- getMouseIsOverBox mv btnSize
      if isOver
        then do renderBtnOver
                -- we can't switch cursors because this gets run on every button
                -- that gets rendered, causing only the last rendered button to
                -- have real effect. when we switch out to using some kind of
                -- quadtree-backed hit-testing we can enable this
                --switchCursor Raw.SDL_SYSTEM_CURSOR_HAND
                updateBtn ButtonStateOver
        else do renderBtnUp
                --switchCursor Raw.SDL_SYSTEM_CURSOR_ARROW
                return ButtonStateUp
--------------------------------------------------------------------------------
-- Button properties
--------------------------------------------------------------------------------
sizeOfButton :: MonadIO m => Slot Button -> m (V2 Float)
sizeOfButton = flip fromSlot btnSize
