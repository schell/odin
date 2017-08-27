{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Devel.Utils where

import           Control.Monad.IO.Class (MonadIO (..))
import           Odin.Engine
import           Odin.Engine.Checkpoint
import           System.Exit            (exitFailure)
import           System.FilePath        (FilePath, (</>))

getWindow :: MonadIO m => m SDL2Backends
getWindow = withCheckpoint "window" startWindow $ \case
  Left err       -> liftIO (putStrLn err >> exitFailure)
  Right backends -> return backends
  where startWindow = liftIO $ startupWindow (V2 800 600) "odin-engine-exe"

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

incrementRecomps :: MonadIO m => m Int
incrementRecomps = withCheckpoint "recomps" (pure (0 :: Int)) $ \n -> do
  updateCheckpoint "recomps" $ succ n
  return n

--persistentAllocations :: Checkpoint Allocated
--persistentAllocations = "persistent-allocations"
--
--persistAllocations :: (MonadSafe m, MonadIO m) => m ()
--persistAllocations = do
--  allocs :: Allocated <- get
--  withCheckpoint persistentAllocations (pure AllocatedNone) $ const $
--    updateCheckpoint persistentAllocations allocs
--
--destroyAllocations :: MonadIO m => m ()
--destroyAllocations =
--  withCheckpoint persistentAllocations (pure AllocatedNone) $ \allocs -> do
--    liftIO $ putStrLn $ unwords [ "Deallocating"
--                                , show $ countAllocated allocs
--                                , "from a previous compilation."
--                                ]
--    deallocAllAllocated allocs

pad :: Int -> String -> String
pad n = unlines . map (padding ++) . lines
  where padding = replicate n ' '
