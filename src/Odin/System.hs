{-# LANGUAGE OverloadedStrings #-}
module Odin.System where

import Graphics.Text.TrueType
import System.Directory
import System.FilePath

odinFont :: FilePath -> IO (Either String Font)
odinFont fp = do
    cwd <- getCurrentDirectory
    let path = cwd </> "fonts" </> fp
    loadFontFile path
