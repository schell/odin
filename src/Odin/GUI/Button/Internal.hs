{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Odin.GUI.Button.Internal where

import Control.Monad.Evented
import Gelatin.FreeType2
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
  { btnDataAtlas     :: Atlas
  , btnDataStr       :: String
  , btnDataGlyphSize :: GlyphSize
  }

data ButtonView = ButtonView
  { btnData    :: ButtonData
  , btnMailbox :: Mailbox ButtonState
  , btnPainter :: Painter (ButtonData, ButtonState) System
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

buttonLifeCycle :: ButtonView -> Entity -> Evented ()
buttonLifeCycle ButtonView{..} k = do
  let ButtonData{..} = btnData
  -- Alloc all our renderers up front
  (sz,rs,d) <- lift $ do
    io $ putStrLn "about to size"
    sz   <- runPainterSize btnPainter (btnData, ButtonStateUp)
    liftIO $ print sz
    up   <- compilePainter btnPainter (btnData, ButtonStateUp)
    ovr  <- compilePainter btnPainter (btnData, ButtonStateOver)
    down <- compilePainter btnPainter (btnData, ButtonStateDown)
    let d  = mapM_ fst [up,ovr,down]
        rs = ButtonRndrs (snd up) (snd ovr) (snd down)
    return (sz,rs,d)
  lift $ k .# dloc d #. rndr (btnRndrsUp rs)

  let upCycle = do
        -- Wait until the mouse is over the button
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
allocButtonView :: Atlas -> String -> GlyphSize
                -> Painter (ButtonData, ButtonState) System
                -> (ButtonState -> System ())
                -> System ButtonView
allocButtonView atlas str px painter onRecv = do
  mb <- mailbox
  recv mb onRecv
  return $ ButtonView (ButtonData atlas str px) mb painter

-- | Creates a fresh button entity in two dimensions.
freshButton :: V2 Float -> ButtonView -> System Entity
freshButton p bview = do
  k <- fresh
  k .# pos p
    ## script [Script $ runEventedScript $ buttonLifeCycle bview k]
