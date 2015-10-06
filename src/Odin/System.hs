module Odin.System where

import Odin.Graphics.Types

import Gelatin.Core.Rendering
import Graphics.UI.GLFW
import Graphics.Text.TrueType
import System.Directory
import System.FilePath

odinRez :: Window -> IO Rez
odinRez w = do
    grs <- loadGeomRenderSource
    brs <- loadBezRenderSource
    mrs <- loadMaskRenderSource
    Right hack <- odinFont "Hack-Regular.ttf"
    efawe <- odinFont "fontawesome-webfont.ttf"
    fawe <- case efawe of
        Left str -> putStrLn str >> return hack
        Right fawe -> return fawe
    return $ Rez grs brs mrs w hack fawe

odinFont :: FilePath -> IO (Either String Font)
odinFont fp = do
    cwd <- getCurrentDirectory
    let path = cwd </> "fonts" </> fp
    loadFontFile path


