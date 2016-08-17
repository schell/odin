{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Odin.Core.System (
    runSystem
  , tickTime
  , tickEvents
  , tickScripts
  , tickRender
  , tickSystem
  , tickEmbedded
  , freshSystem
  ) where

import           Gelatin.SDL2 hiding (E)
import           SDL hiding (Event, get, time)
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import           Data.Monoid ((<>))
import           Data.Foldable (foldl')
import           Control.Lens
import           System.Exit (exitSuccess)

import           Odin.Core.Common
import           Odin.Core.Component

isQuit :: Keysym -> Bool
isQuit (Keysym (Scancode 20) (Keycode 113) m) = any ($ m)
    [ keyModifierLeftCtrl
    , keyModifierRightCtrl
    , keyModifierLeftGUI
    , keyModifierRightGUI
    ]
isQuit _ = False


tickTime :: (Time s m, DoesIO m) => m ()
tickTime = do
  lastT <- use (time.timeLast)
  t <- io ticks
  time.timeLast .= t
  time.timeDelta .= t - lastT

tickEvents :: (Events s m, DoesIO m) => m ()
tickEvents = do
  evs <- io (pollEvents >>= mapM (processEvent . eventPayload))
  events .= evs
  where processEvent QuitEvent = exitSuccess
        processEvent ev@(KeyboardEvent (KeyboardEventData _ _ _ k)) = do
          when (isQuit k) exitSuccess
          --print (m,r,k)
          return ev
        processEvent e = return e

runScripts :: [Script] -> System [Script]
runScripts scripts0 = do
  scripts1 <- mapM runScript scripts0
  return $ filter isRunningScript scripts1

tickScripts :: System ()
tickScripts = do
  -- Get all the current scripts to run
  ss <- use scripts
  -- Clear out the scripts because running the current set of scripts will
  -- possibly add new scripts
  scripts .= mempty
  -- Run the scripts and filter to remove any dead ones
  results <- sequence (runScripts <$> ss)
  -- Add the remaining scripts on the end of any new ones
  scripts %= IM.unionWith (++) results

tickCommands :: System ()
tickCommands = do
  cmds <- use commands
  commands .= []
  mapM_ runCommand cmds
    where runCommand (SystemDeleteEntity k) = do
            tfrms.at k                   .= Nothing
            scene.scWorld.worldObjs.at k .= Nothing
            names.at k                   .= Nothing
            scripts.at k                 .= Nothing
            rndrs.at k                   .= Nothing
            dealloc k
            deallocs.at k                .= Nothing

tickPhysics :: (Physics s m, Time s m, DoesIO m) => m ()
tickPhysics = do
  oscene <- use scene
  -- Time is in milliseconds
  dt <- use (time.timeDelta)
  t0 <- use (time.timeLeft)
  let tt = dt + t0
      -- one physics step should be 0.01
      n = floor (fromIntegral tt / 10 :: Double)
      t1 = tt - (fromIntegral n * 10)
  time.timeLeft .= t1
  scene.scWorld .= runWorldOver 0.01 oscene n

tickAllExceptRender :: System ()
tickAllExceptRender = do
  tickTime
  tickEvents
  tickScripts
  tickCommands
  skipPhysics <- use (options.contains SystemSkipPhysicsTick)
  unless skipPhysics tickPhysics

renderIntersecting :: IntMap RenderIO -> IntMap PictureTransform
                   -> PictureTransform -> IO ()
renderIntersecting renderers transforms t = do
  let appliedTfrms = (t <>) <$> transforms
      m = IM.intersectionWith ($) renderers appliedTfrms
  sequence_ m

tickRender :: System ()
tickRender = do
  rs     <- use rndrs
  ts0    <- use tfrms
  objs   <- use (scene.scWorld.worldObjs)
  let objTs = worldObj2PicTfrm <$> objs
      ts1   = IM.unionWith (<>) ts0 objTs
  ask >>= io . clearFrame
  io $ renderIntersecting rs ts1 mempty
  ask >>= io . updateWindowSDL2

tickSystem :: System ()
tickSystem = do
  tickAllExceptRender
  tickRender

tickEmbedded :: (DoesIO m
                ,Rndrs s m
                ,Deallocs s m
                ) => Entity -> Sys -> m Script
tickEmbedded k step = do
  next@Sys{..} <- io $ execStateT tickAllExceptRender step
  -- Set its render to run all of the step's renderings
  rndrs.at k .= (Just $ renderIntersecting _sysRndrs _sysTfrms)
  -- Set its dealloc to run all of the step's deallocations
  deallocs.at k .= (Just $ sequence_ _sysDeallocs)
  return $ Script $ tickEmbedded k next

-- | Alloc a fresh entity component system.
-- Nests an encapsulated System within the current System.
-- Using this function we can spawn sub systems which don't interact.
freshSystem :: System () -> System Entity
freshSystem f = do
  k     <- fresh
  rz    <- ask
  win   <- ask
  lst   <- io ticks
  let t  = SystemTime lst 0 0
      ss = IM.singleton k [Script $ f >> return ScriptEnd]
      step = (emptySys rz win){ _sysScripts = ss, _sysTime = t }
  k .# tfrm mempty
    ## script [Script $ tickEmbedded k step]

runSystem :: Sys -> System a -> IO a
runSystem = flip evalStateT
