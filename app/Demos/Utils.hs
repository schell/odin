{-# LANGUAGE LambdaCase #-}
module Demos.Utils where

import Gelatin.Fruity

import System.FilePath
import System.Directory

-- | Load our standard fonts.
getFont :: String -> IO (Either String Font)
getFont fontname = do
    -- Get our fonts
    assets <- (</> "assets") <$> getCurrentDirectory
    -- Load our header font
    let font = assets </> "fonts" </> fontname
    loadFontFile font
