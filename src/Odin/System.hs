{-# LANGUAGE OverloadedStrings #-}
module Odin.System where

import Odin.Data.Common

import Gelatin.Core.Rendering
import Graphics.UI.GLFW
import Graphics.Text.TrueType
import System.Directory
import System.FilePath

odinRez :: Window -> IO Rez
odinRez w = do
    sh <- loadShaders
    fc <- compileFontCache
    return $ Rez sh w fc

odinFont :: FilePath -> IO (Either String Font)
odinFont fp = do
    cwd <- getCurrentDirectory
    let path = cwd </> "fonts" </> fp
    loadFontFile path
