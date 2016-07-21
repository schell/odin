{-# LANGUAGE LambdaCase #-}
module Demos.Utils where

import Gelatin.SDL2

import System.FilePath
import System.Directory

-- | Load our standard fonts.
getFont :: String -> IO (Either String FontData)
getFont fontname = do
    -- Get our fonts
    assets <- (</> "assets") <$> getCurrentDirectory
    -- Load our header font
    let font = assets </> "fonts" </> fontname
    loadFont font
