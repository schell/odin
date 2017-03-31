{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Devel.Utils where

import           Odin.Engine
import           Odin.Engine.Checkpoint
import           System.Exit            (exitFailure)
import           System.FilePath        (FilePath, (</>))

getWindow :: IO SDL2Backends
getWindow = runM $ withCheckpoint "window" startWindow $ \case
  Left err       -> io (putStrLn err >> exitFailure)
  Right backends -> return backends
  where startWindow = io $ startupWindow (V2 800 600) "odin-engine-exe"

assetsDir :: FilePath
assetsDir = ".." </> "assets"

fontsDir :: FilePath
fontsDir = assetsDir </> "fonts"

defaultFont :: DefaultFont
defaultFont = DefaultFont $ fontDescriptor (fontsDir </> "KMKDSP__.ttf")  16

monoFont :: DefaultFont
monoFont = DefaultFont $ fontDescriptor (fontsDir </> "Inconsolata-Regular.ttf") 12

iconFont :: IconFont
iconFont = IconFont $ fontDescriptor (fontsDir </> "FontAwesome.otf") 16

incrementRecomps :: Member IO r => Eff r Int
incrementRecomps = withCheckpoint "recomps" (pure (0 :: Int)) $ \n -> do
  updateCheckpoint "recomps" $ succ n
  return n

persistentAllocations :: Checkpoint Allocated
persistentAllocations = "persistent-allocations"

persistAllocations :: (Member (State Allocated) r, Member IO r) => Eff r ()
persistAllocations = do
  allocs :: Allocated <- get
  withCheckpoint persistentAllocations (pure AllocatedNone) $ const $
    updateCheckpoint persistentAllocations allocs

destroyAllocations :: Member IO r => Eff r ()
destroyAllocations =
  withCheckpoint persistentAllocations (pure AllocatedNone) $ \allocs -> do
    io $ putStrLn $ unwords [ "Deallocating"
                            , show $ countAllocated allocs
                            , "from a previous compilation."
                            ]
    deallocAllAllocated allocs

pad :: Int -> String -> String
pad n = unlines . map (padding ++) . lines
  where padding = replicate n ' '
