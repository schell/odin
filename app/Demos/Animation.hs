{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Demos.Animation
  ( AnimationDemo(..)
  , allocDemo
  , renderDemo
  , freeDemo
  ) where

import Control.Lens hiding (to)
import Control.Varying
import Linear hiding (rotate)

import Gelatin hiding (move,rotate)
import Data.Char.FontAwesome
import Odin.Core
import Odin.GUI.Text.Internal
import Odin.GUI.Button.Internal
import Odin.GUI.Picture
import Odin.GUI.Animation.Internal
import Odin.GUI.Styles

data AnimationDemo m = AnimationDemo { demoRenderer :: (m (), [RenderTransform] -> m ())
                                     , pauseDemo    :: m ()
                                     , resumeDemo   :: m ()
                                     }

data MyTween = MyTween { tweenPos   :: V2 Float
                       , tweenScale :: V2 Float
                       , tweenMultiply :: V4 Float
                       }

posTween :: Monad m => VarT m Float (V2 Float)
posTween = V2 <$> x <*> y
  where x  = flip tweenStream 0 $ fix $ \nxt -> do
               tween_ easeInExpo 0 100 1
               tween_ easeOutExpo 100 200 1
               constant 200 1
               tween_ easeInExpo 200 100 1
               tween_ easeOutExpo 100 0 1
               constant 0 1
               step 0
               nxt
        y  = flip tweenStream 0 $ fix $ \nxt -> do
               constant 0 1
               tween_ easeInExpo 0 100 1
               tween_ easeOutExpo 100 200 1
               constant 200 1
               tween_ easeInExpo 200 100 1
               tween_ easeOutExpo 100 0 1
               step 0
               nxt

scaleTween :: Monad m => VarT m Float (V2 Float)
scaleTween = V2 <$> x <*> y
  where x = flip tweenStream 1 $ fix $ \nxt -> do
              tween_ easeInExpo 1 2 1
              tween_ easeOutExpo 2 4 1
              tween_ easeInExpo 4 2 1
              tween_ easeOutExpo 2 1 1
              nxt
        y = flip tweenStream 1 $ fix $ \nxt -> do
              tween_ easeInExpo 1 2 1
              tween_ easeOutExpo 2 4 1
              tween_ easeInExpo 4 2 1
              tween_ easeOutExpo 2 1 1
              nxt

colorTween :: Monad m => VarT m Float (V4 Float)
colorTween = V4 <$> r <*> g <*> b <*> pure 1
  where r = flip tweenStream 1 $ fix $ \nxt -> do
              step 1
              tween easeInExpo 1 0 1
              constant 0 2
              tween easeInExpo 0 1 1
              constant 1 2
              nxt
        g = flip tweenStream 1 $ fix $ \nxt -> do
              step 1
              constant 1 1
              tween easeInExpo 1 0 1
              constant 0 2
              tween easeInExpo 0 1 1
              constant 1 1
              step 1
              nxt
        b = flip tweenStream 1 $ fix $ \nxt -> do
              step 1
              constant 1 2
              tween easeInExpo 1 0 1
              constant 0 2
              tween easeInExpo 0 1 1
              step 1
              step 1
              step 1
              nxt

myTween :: Monad m => VarT m Float [RenderTransform]
myTween = sequenceA [moveV2     <$> posTween
                    ,scaleV2    <$> scaleTween
                    ,multiplyV4 <$> colorTween
                    ]

allocDemo :: (MonadIO m, Rezed s m, Fonts s m, Time s m, Resources s m)
          => m (AnimationDemo m)
allocDemo = do
  comicFont <- getFontPath "KMKDSP__.ttf"
  let cdesc = fontDescriptor comicFont 16
  title      <- allocText cdesc white "Animation Demo"
  playBtn    <- allocButton (iconButtonPainter 32) [faPlay]
  pauseBtn   <- allocButton (iconButtonPainter 32) [faPause]
  pause      <- slot False
  anime      <- allocAnime myTween
  lastRs     <- slot []
  (_,square) <- allocColorPicture $
    setGeometry $ fan $ mapVertices (,white) $ rectangle (-10) 10

  V2 tw _    <- sizeOfText title
  V2 playw _ <- sizeOfButton playBtn

  let render rs = do
        renderText title $ move 0 16 : rs
        isPaused <- unslot pause
        stillPaused <- if isPaused
          then (renderButton playBtn $ move (tw + 4) 0 : rs) >>= \case
            ButtonStateClicked -> return False
            _ -> return isPaused
          else (renderButton pauseBtn $ move (tw + 4) 0 : rs) >>= \case
            ButtonStateClicked -> return True
            _ -> return isPaused
        unless stillPaused $
          stepAnime anime >>= reslot lastRs
        rrs   <- unslot lastRs
        (_,rnd) <- unslot square
        io $ rnd $ rrs ++ rs
        pause `is` stillPaused

      free = do
        freeText title
        freeButton playBtn
        freeButton pauseBtn
  return
    AnimationDemo{ demoRenderer = (free, render)
                 , pauseDemo = pause `is` True
                 , resumeDemo = pause `is` False
                 }

renderDemo :: MonadIO m => AnimationDemo m -> [RenderTransform] -> m ()
renderDemo a rs = snd (demoRenderer a) rs

freeDemo :: MonadIO m => AnimationDemo m -> m ()
freeDemo a = fst $ demoRenderer a
