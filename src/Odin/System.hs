{-# LANGUAGE OverloadedStrings #-}
module Odin.System where

import Odin.Data.Common

import Gelatin.Core.Rendering
import Graphics.UI.GLFW
import Graphics.Text.TrueType
import System.Directory
import System.FilePath
import qualified Data.Map.Strict as M

odinRez :: Window -> IO Rez
odinRez w = do
    sh <- loadShaders
    fc <- compileFontCache
    Right hack <- odinFont "Hack-Regular.ttf"
    efawe <- odinFont "fontawesome-webfont.ttf"
    fawe <- case efawe of
        Left str -> putStrLn str >> return hack
        Right fawe -> return fawe
    let m = M.fromList
            [ (FontDescriptor "Hack" $ FontStyle False False, hack)
            , (FontDescriptor "FontAwesome" $ FontStyle False False, fawe)
            ]
    return $ Rez sh w fc m

odinFont :: FilePath -> IO (Either String Font)
odinFont fp = do
    cwd <- getCurrentDirectory
    let path = cwd </> "fonts" </> fp
    loadFontFile path
