{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Odin.System (
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
--import qualified Data.Set as S
--import           Data.Set (Set)
import           Data.Monoid ((<>))
import           Control.Monad (when, unless)
import           Control.Monad.Freer.Internal (runM)
import           System.Exit (exitSuccess)

import           App.Framework (isQuit)
import           Odin.Common
import           Odin.Component
--import           Odin.Physics

tickTime :: (Modifies Time r, DoesIO r) => Eff r ()
tickTime = do
  Time lastT _ lt <- get
  t <- io ticks
  let dt = fromIntegral (t - lastT) / 1000
  put $ Time t dt lt

tickEvents :: (Modifies [EventPayload] r, DoesIO r) => Eff r ()
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

tickPhysics :: (Modifies OdinScene r
               ,Modifies Time r
               ,DoesIO r
               ) => Eff r ()
tickPhysics = do
  scene <- getScene
  Time stamp dt t0 <- get
  let tt = dt + t0
      n = floor $ tt / 0.01
      t1 = tt - (fromIntegral n * 0.01)
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

tickRender :: ( ModifiesComponent RenderIO r
              , ModifiesComponent PictureTransform r
              , Modifies OdinScene r
              , Reads Rez r
              , Reads Window r
              , DoesIO r
              ) => Eff r ()
tickRender = do
  rs     <- getRenderers
  ts0    <- getPicTransforms
  --objs   <- getWorldObjects
  --let ts1 = applyPhysics objs ts0
  ask >>= io . clearFrame
  io $ renderIntersecting rs ts0 mempty
  ask >>= io . updateWindowSDL2

tickSystem :: System ()
tickSystem = do
  tickAllExceptRender
  tickRender

type SystemTuple =
  ( ( ( ( ( ( ( ( ( ( Int
                    , IntMap Name)
                  , IntMap PictureTransform)
                , IntMap RenderIO)
              , IntMap DeallocIO)
            , IntMap [Script])
          , [EventPayload])
        , Time)
      , OdinScene)
    , SystemOptions)
  , SystemCommands)

tupleToStep :: Window -> Rez -> SystemTuple -> SystemStep
tupleToStep win rez p =
  SystemStep names tfrms rndrs deallocs scrps k win rez evs t scene opts cmds
  where k        = fst $ fst $ fst $ fst $ fst $ fst $ fst $ fst $ fst $ fst p
        names    = snd $ fst $ fst $ fst $ fst $ fst $ fst $ fst $ fst $ fst p
        tfrms    = snd $ fst $ fst $ fst $ fst $ fst $ fst $ fst $ fst p
        rndrs    = snd $ fst $ fst $ fst $ fst $ fst $ fst $ fst p
        deallocs = snd $ fst $ fst $ fst $ fst $ fst $ fst p
        scrps    = snd $ fst $ fst $ fst $ fst $ fst p
        evs      = snd $ fst $ fst $ fst $ fst p
        t        = snd $ fst $ fst $ fst p
        scene    = snd $ fst $ fst p
        opts     = snd $ fst p
        cmds     = snd p

runSystem :: SystemStep -> System () -> IO SystemStep
runSystem SystemStep{..} f = tupleToStep sysWindow sysRez <$>
  runM
   ( flip runState  sysCommands
   $ flip runState  sysOptions
   $ flip runState  sysScene
   $ flip runState  sysTime
   $ flip runState  sysEvents
   $ flip runReader sysRez
   $ flip runReader sysWindow
   $ flip runFresh' sysFresh
   $ flip runState  sysScripts
   $ flip runState  sysDealloc
   $ flip runState  sysRndrs
   $ flip runState  sysTfrms
   $ runState g     sysNames)
     where g = f >> fresh

tickEmbedded :: (DoesIO r
                ,ModifiesComponent RenderIO r
                ,ModifiesComponent DeallocIO r
                ) => Entity -> SystemStep -> Eff r Script
tickEmbedded actor step = do
  next@SystemStep{..} <- io $ runSystem step tickAllExceptRender
  actor `setRenderer` renderIntersecting sysRndrs sysTfrms
  -- Set it's dealloc to run all of the step's deallocations
  actor `setDealloc` sequence_ sysDealloc
  return $ Script $ tickEmbedded actor next

-- | Alloc a fresh entity component system.
-- Nests an encapsulated System within the current System.
-- Using this function we can spawn sub systems which don't interact.
freshSystem :: (MakesEntities r
               ,ModifiesComponent PictureTransform r
               ,ModifiesComponent [Script] r
               ,Reads Rez r
               ,Reads Window r
               ,DoesIO r
               ) => System () -> Eff r Entity
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
