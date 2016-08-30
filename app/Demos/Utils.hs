{-# LANGUAGE LambdaCase #-}
module Demos.Utils where

import System.FilePath
import System.Directory

getFontPath :: String -> IO FilePath
getFontPath fontname =
  (</> "assets" </> "fonts" </> fontname) <$> getCurrentDirectory
