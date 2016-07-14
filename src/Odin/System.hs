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
  , embedSystem
  , freshSystem
  ) where

import           Gelatin.Picture
import           Gelatin.SDL2 hiding (E)
import           SDL hiding (Event, get)
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import           Data.Monoid ((<>))
import           Control.Monad (when)
import           Control.Monad.Freer.Internal (runM)
import           System.Exit (exitSuccess)

import           App.Framework (isQuit)
import           Odin.Common
import           Odin.Component
import           Odin.Physics

tickTime :: (Modifies Time r, DoesIO r) => Eff r ()
tickTime = do
  Time lastT _ <- get
  t            <- io ticks
  let dt = fromIntegral (t - lastT) / 1000
  put $ Time t dt

tickEvents :: (Modifies [EventPayload] r, DoesIO r) => Eff r ()
tickEvents = io (pollEvents >>= mapM (processEvent . eventPayload)) >>= put
  where processEvent QuitEvent = exitSuccess
        processEvent ev@(KeyboardEvent (KeyboardEventData _ _ _ k)) = do
          when (isQuit k) exitSuccess
          --print (m,r,k)
          return ev
        processEvent e = return e

tickScripts :: System ()
tickScripts = do
  -- Get the current scripts to run
  scripts <- getScripts
  -- Clear out the scripts because running the current set of scripts will
  -- possibly add new scripts
  put ([] :: [Script])
  -- Run the scripts and filter to remove any dead ones
  results <- mapM runScript scripts
  let remaining = filter isRunningScript results
  -- Add the remaining scripts on the end of any new ones
  modify (++ remaining)

tickTimeEventsAndScripts :: System ()
tickTimeEventsAndScripts = tickTime >> tickEvents >> tickScripts

-- | Runs one script and adds it to the system's scripts if it has not concluded.
performScript :: Script -> System ()
performScript ScriptEnd = return ()
performScript (Script s) = s >>= \case
  ScriptEnd -> return ()
  script    -> addScripts [script]

renderIntersecting :: IntMap RenderIO -> IntMap PictureTransform -> PictureTransform -> IO ()
renderIntersecting renderers transforms tfrm = do
  let appliedTfrms = (tfrm <>) <$> transforms
      m = IM.intersectionWith ($) renderers appliedTfrms
  sequence_ m

tickRender :: ( ModifiesComponent RenderIO r
              , ModifiesComponent PictureTransform r
              , Reads Rez r
              , Reads Window r
              , DoesIO r
              ) => Eff r ()
tickRender = do
  rs     <- getRenderers
  ts     <- getTransforms
  ask >>= io . clearFrame
  io $ renderIntersecting rs ts mempty
  ask >>= io . updateWindowSDL2

tickSystem :: System ()
tickSystem = do
  tickTimeEventsAndScripts
  tickRender

type SystemTuple =
  ( ( ( ( ( ( ( Int
              , IntMap Name)
            , IntMap PictureTransform)
          , IntMap RenderIO)
        , IntMap DeallocIO)
      , [EventPayload])
    , Time)
  , [Script])

tupleToStep :: Window -> Rez -> SystemTuple -> SystemStep
tupleToStep win rez p =
  SystemStep names tfrms rndrs deallocs k win rez evs t scrps
  where k        = fst $ fst $ fst $ fst $ fst $ fst $ fst p
        names    = snd $ fst $ fst $ fst $ fst $ fst $ fst p
        tfrms    = snd $ fst $ fst $ fst $ fst $ fst p
        rndrs    = snd $ fst $ fst $ fst $ fst p
        deallocs = snd $ fst $ fst $ fst p
        evs      = snd $ fst $ fst p
        t        = snd $ fst p
        scrps    = snd p

runSystem :: SystemStep -> System () -> IO SystemStep
runSystem SystemStep{..} f = tupleToStep sysWindow sysRez <$>
  runM
   ( flip runState  sysScripts
   $ flip runState  sysTime
   $ flip runState  sysEvents
   $ flip runReader sysRez
   $ flip runReader sysWindow
   $ flip runFresh' sysFresh
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
  next@SystemStep{..} <- io $ runSystem step tickTimeEventsAndScripts
  actor `setRenderer` renderIntersecting sysRndrs sysTfrms
  -- Set it's dealloc to run all of the step's deallocations
  actor `setDealloc` sequence_ sysDealloc
  return $ Script $ tickEmbedded actor next

-- | Nests an encapsulated System within the current System.
-- Using this function we can spawn sub components which don't interact.
embedSystem :: (MakesEntities r
               ,ModifiesComponent PictureTransform r
               ,Modifies [Script] r
               ) => SystemStep -> Eff r Entity
embedSystem step = do
  -- Create an entity for this system
  actor <- fresh
  -- Set it's transform to identity
  actor `setPicTransform` mempty
  addScript (tickEmbedded actor step)
  return actor

-- | Alloc a fresh entity component system.
freshSystem :: (MakesEntities r
               ,ModifiesComponent PictureTransform r
               ,Modifies [Script] r
               ,Reads Rez r
               ,Reads Window r
               ) => System () -> Eff r Entity
freshSystem f = do
  rez <- ask
  win <- ask
  embedSystem (emptySystemStep rez win){ sysScripts = scripts }
    where scripts = [Script $ f >> return ScriptEnd]
