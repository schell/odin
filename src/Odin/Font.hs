{-# LANGUAGE OverloadedStrings #-}
module Odin.Font where

import Gelatin.GLFW
import System.Directory
import System.FilePath

odinFont :: FilePath -> IO (Either String Font)
odinFont fp = do
    cwd <- getCurrentDirectory
    let path = cwd </> "fonts" </> fp
    loadFontFile path

arialDescriptor :: FontDescriptor
arialDescriptor = FontDescriptor "Arial" $ FontStyle False False

hackDescriptor :: FontDescriptor
hackDescriptor = FontDescriptor "Hack" $ FontStyle False False
