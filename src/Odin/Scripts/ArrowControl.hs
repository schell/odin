{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Odin.Scripts.ArrowControl (arrowControl) where

import Gelatin.SDL2
import SDL
import Data.Monoid ((<>))
import Data.Maybe (mapMaybe)
import Control.Lens

import Odin.Core

data Direction = North | East | South | West deriving (Show, Eq, Bounded)

codeToDirection :: Scancode -> Maybe Direction
codeToDirection ScancodeUp = Just North
codeToDirection ScancodeRight = Just East
codeToDirection ScancodeDown = Just South
codeToDirection ScancodeLeft = Just West
codeToDirection _ = Nothing

directionToCode :: Direction -> Scancode
directionToCode North = ScancodeUp
directionToCode East  = ScancodeRight
directionToCode South = ScancodeDown
directionToCode West  = ScancodeLeft

directionToV2 :: Direction -> V2 Float
directionToV2 North = V2 0 (-1)
directionToV2 East = V2 1 0
directionToV2 South = V2 0 1
directionToV2 West = V2 (-1) 0

arrowCodes :: [Scancode]
arrowCodes = [ScancodeUp, ScancodeLeft, ScancodeDown, ScancodeRight]

onTrue :: (a -> Bool) -> a -> Maybe a
onTrue f x = if f x then Just x else Nothing

arrowControl :: (Events s m
                ,Scripts s m
                ,Tfrms s m
                ,Time s m
                ) => Entity -> m Script
arrowControl actor = do
  -- First wait until the user presses an arrow key
      -- For that we'll need some scafolding so we can test and extract the
      -- arrow direction
  let isArrowPressed (KeyboardEvent (KeyboardEventData _ Pressed False Keysym{..})) =
        msum $ map (onTrue (== keysymScancode)) arrowCodes
      isArrowPressed _ = Nothing
  evs <- use events
  let codes = mapMaybe isArrowPressed evs
  unless (null codes) $ do
    let dirs = mapMaybe codeToDirection codes
    -- If we got a direction then apply the arrow move script to each
    -- direction.
    unless (null dirs) $ do
      ss <- mapM (arrowControlMove actor) dirs
      scripts.at actor %= Just . (maybe ss (++ ss))
  nextScript $ arrowControl actor

arrowControlMove :: (Events s m
                    ,Tfrms s m
                    ,Time s m
                    ) => Entity -> Direction -> m Script
arrowControlMove actor dir = do
  -- Update the transform of the actom
  dt <- readTimeDeltaSeconds
  let t = PictureTransform (mat4Translate $ promoteV2 $ dt * 100 *^ directionToV2 dir) 1 1
  tfrms.at actor %= Just . (maybe t (t <>))
  -- Find if the arrow key was released
  let isArrowReleased (KeyboardEvent (KeyboardEventData _ Released False Keysym{..})) =
        keysymScancode == directionToCode dir
      isArrowReleased _ = False
  released <- any isArrowReleased <$> (use events)
  --  Restart the process all over again, from the top
  if released
    then endScript
    else nextScript $ arrowControlMove actor dir
