{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Odin.GUI.Text.Internal where

import Gelatin.FreeType2
import Gelatin.SDL2
import Control.Lens
import Odin.Core
import Odin.GUI.Common

data Text = Text
  { txtSize :: V2 Float
  , txtRndr :: GLRenderer
  }
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
compileText :: (MonadIO m, Rezed s m, Fonts s m)
            => FontDescriptor -> V4 Float -> String -> m Text
compileText desc color str = loadAtlas desc asciiChars >>= \case
  Nothing -> do
    io $ putStrLn "ERROR ALLOCING TEXT!"
    return $ Text 0 mempty
  Just atlas0 -> do
    rz  <- use rez
    (r,sz,atlas) <- freetypeGLRenderer rz atlas0 color str
    saveAtlas atlas
    return $ Text sz r

allocText :: (MonadIO m, Rezed s m, Fonts s m, Resources s m)
          => FontDescriptor -> V4 Float -> String -> m (Slot Text)
allocText desc color str = do
  txt <- compileText desc color str
  s   <- allocSlot txt
  registerFree $ freeText s
  return s

reallocText :: (MonadIO m, Rezed s m, Fonts s m)
            => Slot Text -> FontDescriptor -> V4 Float -> String -> m ()
reallocText s desc color str = compileText desc color str >>= swapSlot s

freeText :: (MonadIO m) => Slot Text -> m ()
freeText s = fromSlot s (fst . txtRndr) >>= io

renderText :: MonadIO m => Slot Text -> [RenderTransform] -> m ()
renderText s rs = do
  Text{..} <- readSlot s
  io $ snd txtRndr rs

sizeOfText :: MonadIO m => Slot Text -> m (V2 Float)
sizeOfText = flip fromSlot txtSize
