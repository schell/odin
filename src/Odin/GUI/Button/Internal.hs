{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.GUI.Button.Internal where

import Control.Monad.Evented
import Gelatin.Fruity
import Gelatin.SDL2
import Control.Lens
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
  { btnDataFont      :: Font
  , btnDataStr       :: String
  , btnDataPointSize :: Float
  }

data ButtonView t s r v = ButtonView
  { btnData    :: ButtonData
  , btnMailbox :: Mailbox ButtonState
  , btnPainter :: Painter (ButtonData, ButtonState) t s r v
  , btnCompiler:: Picture t s r v () -> System GLRenderer
  }

data ButtonRndrs = ButtonRndrs { btnRndrsUp   :: RenderIO
                               , btnRndrsOver :: RenderIO
                               , btnRndrsDown :: RenderIO
                               }
--------------------------------------------------------------------------------
-- A Button's life cycle
--------------------------------------------------------------------------------
getMouseIsOutEntityWithSize :: Entity -> V2 Float -> System Bool
getMouseIsOutEntityWithSize k sz = not <$> getMouseIsOverEntityWithSize k sz

buttonLifeCycle :: (Unbox v, Monoid (PictureData t (V2 Float) r v))
                => ButtonView t (V2 Float) r v -> Entity -> Evented ()
buttonLifeCycle ButtonView{..} k = do
  let ButtonData{..} = btnData
  -- Alloc all our renderers up front
  (sz,rs,d) <- lift $ do
    let sz = pictureSize' $ btnPainter (btnData, ButtonStateUp)
    up   <- btnCompiler $ btnPainter (btnData, ButtonStateUp)
    ovr  <- btnCompiler $ btnPainter (btnData, ButtonStateOver)
    down <- btnCompiler $ btnPainter (btnData, ButtonStateDown)
    let d  = mapM_ fst [up,ovr,down]
        rs = ButtonRndrs (snd up) (snd ovr) (snd down)
    return (sz,rs,d)
  lift $ k .# dloc d #. rndr (btnRndrsUp rs)

  let upCycle = do
        -- Wait until mouse is over the button
        waitUntil $ getMouseIsOverEntityWithSize k sz
        -- Set the button renderer to 'over' and send the 'over' message through to
        -- the mailbox
        lift $ do rndrs.at k .= Just (btnRndrsOver rs)
                  send btnMailbox ButtonStateOver
        -- Run the 'over' cycle
        overCycle

      overCycle = do
        -- Wait till either the mouse goes out of the button bounds or the left
        -- mouse button is down (inside, which we already know)
        e <- waitUntilEither (($ ButtonLeft) <$> io getMouseButtons)
                             (getMouseIsOutEntityWithSize k sz)
        case e of
          -- Set the button renderer to 'down', send the 'down' message through
          -- to the mailbox and run the 'down' cycle
          Left ()  -> do lift $ do rndrs.at k .= Just (btnRndrsDown rs)
                                   send btnMailbox ButtonStateDown
                         downCycle
          -- Set the button renderer to 'up', send the 'up' message through to
          -- the mailbox and run the 'up' cycle
          Right () -> do lift $ do rndrs.at k .= Just (btnRndrsUp rs)
                                   send btnMailbox ButtonStateUp
                         upCycle

      downCycle = do
        -- Wait for the left mouse button down
        waitUntil $ (not . ($ ButtonLeft)) <$> io getMouseButtons
        -- Send either a 'clicked' or an 'up' message
        lift $ do getMouseIsOverEntityWithSize k sz >>= \case
                    True  -> send btnMailbox ButtonStateClicked
                    False -> send btnMailbox ButtonStateUp
                  -- Update the renderer to 'up'
                  rndrs.at k .= Just (btnRndrsUp rs)
        -- Rinse and repeat
        upCycle
  -- Start the cycle
  upCycle
--------------------------------------------------------------------------------
-- Alloc'ing Views and Fresh Button Entities
--------------------------------------------------------------------------------
-- Allocs a new button view.
allocButtonView :: Font -> String -> Float
                 -> Painter (ButtonData, ButtonState) t s r v
                 -> (Picture t s r v () -> System GLRenderer)
                 -> (ButtonState -> System ())
                 -> System (ButtonView t s r v)
allocButtonView font str px painter compiler onRecv = do
  mb <- mailbox
  recv mb onRecv
  return $ ButtonView (ButtonData font str px) mb painter compiler

type ColorButtonView   = ButtonView () (V2 Float) Float (V2 Float, V4 Float)
type TextureButtonView = ButtonView GLuint (V2 Float) Float (V2 Float, V2 Float)

-- Allocs a new button view drawn with colors in two dimensions.
allocColorButtonView :: Font -> String -> Float
                      -> ColorPainter (ButtonData, ButtonState)
                      -> (ButtonState -> System ())
                      -> System ColorButtonView
allocColorButtonView font str px painter onRecv =
  allocButtonView font str px painter allocColorPicRenderer onRecv

-- Allocs a new button view drawn with textures in two dimensions.
allocTextureButtonView :: Font -> String -> Float
                      -> TexturePainter (ButtonData, ButtonState)
                      -> (ButtonState -> System ())
                      -> System TextureButtonView
allocTextureButtonView font str px painter onRecv =
  allocButtonView font str px painter allocTexturePicRenderer onRecv

-- | Creates a fresh button entity in two dimensions.
freshButton :: (Unbox v, Monoid (PictureData t (V2 Float) r v))
            => V2 Float -> ButtonView t (V2 Float) r v -> System Entity
freshButton p bview = do
  k <- fresh
  k .# pos p
    ## script [Script $ runEventedScript $ buttonLifeCycle bview k]
