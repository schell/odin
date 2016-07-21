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
  , performScript
  , tickRender
  , tickSystem
  , tickEmbedded
  , freshSystem
  ) where

import           Gelatin.Picture
import           Gelatin.SDL2 hiding (E)
import           SDL hiding (Event, get)
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import           Data.Monoid ((<>))
import           System.Exit (exitSuccess)

import           App.Framework (isQuit)
import           Odin.Core.Common
import           Odin.Core.Component

tickTime :: (Modifies Time m, DoesIO m) => m ()
tickTime = do
  Time lastT _ lt <- get
  t <- io ticks
  let dt = t - lastT
  put $ Time t dt lt

tickEvents :: (Modifies [EventPayload] m, DoesIO m) => m ()
tickEvents = io (pollEvents >>= mapM (processEvent . eventPayload)) >>= put
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
  scripts <- get :: System (IntMap [Script])
  -- Clear out the scripts because running the current set of scripts will
  -- possibly add new scripts
  put (mempty :: IntMap [Script])
  -- Run the scripts and filter to remove any dead ones
  results <- sequence (runScripts <$> scripts)
  -- Add the remaining scripts on the end of any new ones
  modify (IM.unionWith (++) results)

tickCommands :: System ()
tickCommands = do
  -- Get the commands
  cmds <- get :: System SystemCommands
  put ([] :: SystemCommands)
  mapM_ runCommand cmds
    where runCommand (SystemDeleteEntity k) = do deletePicTransform k
                                                 deleteWorldObject k
                                                 deleteName k
                                                 deleteScripts k
                                                 deleteRenderer k
                                                 dealloc k
                                                 deleteDealloc k

tickPhysics :: (Modifies OdinScene m
               ,Modifies Time m
               ,DoesIO m
               ) => m ()
tickPhysics = do
  scene <- getScene
  -- Time is in milliseconds
  Time stamp dt t0 <- get
  let tt = dt + t0
      -- one physics step should be 0.01
      n = floor (fromIntegral tt / 10 :: Double)
      t1 = tt - (fromIntegral n * 10)
      newWorld = runWorldOver 0.01 scene n
      newTime = Time stamp dt t1
  modifyScene $ \s -> s{_scWorld = newWorld}
  put newTime

tickAllExceptRender :: System ()
tickAllExceptRender = do
  tickTime
  tickEvents
  tickScripts
  tickCommands
  skipPhysics <- optionIsSet SystemSkipPhysicsTick
  unless skipPhysics tickPhysics

-- | Runs one script and adds it to the system's scripts if it has not concluded.
performScript :: Entity -> Script -> System ()
performScript _ ScriptEnd = return ()
performScript k (Script s) = s >>= \case
  ScriptEnd -> return ()
  script    -> k `addScripts` [script]

renderIntersecting :: IntMap RenderIO -> IntMap PictureTransform
                   -> PictureTransform -> IO ()
renderIntersecting renderers transforms tfrm = do
  let appliedTfrms = (tfrm <>) <$> transforms
      m = IM.intersectionWith ($) renderers appliedTfrms
  sequence_ m

tickRender :: ( ModifiesComponent RenderIO m
              , ModifiesComponent PictureTransform m
              , Modifies OdinScene m
              , Reads Rez m
              , Reads Window m
              , DoesIO m
              ) => m ()
tickRender = do
  rs     <- getRenderers
  ts0    <- readPicTransforms
  --objs   <- getWorldObjects
  --let ts1 = applyPhysics objs ts0
  ask >>= io . clearFrame
  io $ renderIntersecting rs ts0 mempty
  ask >>= io . updateWindowSDL2

tickSystem :: System ()
tickSystem = do
  tickAllExceptRender
  tickRender

tickEmbedded :: (DoesIO m
                ,ModifiesComponent RenderIO m
                ,ModifiesComponent DeallocIO m
                ) => Entity -> SystemStep -> m Script
tickEmbedded actor step = do
  next@SystemStep{..} <- io $ execStateT tickAllExceptRender step
  actor `setRenderer` renderIntersecting sysRndrs sysTfrms
  -- Set it's dealloc to run all of the step's deallocations
  actor `setDealloc` sequence_ sysDealloc
  return $ Script $ tickEmbedded actor next

-- | Alloc a fresh entity component system.
-- Nests an encapsulated System within the current System.
-- Using this function we can spawn sub systems which don't interact.
freshSystem :: (MakesEntities m
               ,ModifiesComponent PictureTransform m
               ,ModifiesComponent [Script] m
               ,Reads Rez m
               ,Reads Window m
               ,DoesIO m
               ) => System () -> m Entity
freshSystem f = do
  actor <- fresh
  rez   <- ask
  win   <- ask
  lst  <- io ticks
  let t = Time lst 0 0
      scripts = IM.singleton actor [Script $ f >> return ScriptEnd]
      step = (emptySystemStep rez win){ sysScripts = scripts , sysTime = t }
  -- Set it's transform to identity
  actor `setPicTransform` mempty
  actor `addScript` tickEmbedded actor step
  return actor

runSystem :: SystemStep -> System a -> IO a
runSystem = flip evalStateT
