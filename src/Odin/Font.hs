{-# LANGUAGE OverloadedStrings #-}
module Odin.Font where

import Gelatin.GL
import Graphics.Text.TrueType
import System.Directory
import System.FilePath

odinFont :: FilePath -> IO (Either String FontData)
odinFont fp = do
    cwd <- getCurrentDirectory
    let path = cwd </> "fonts" </> fp
    fmap fontyData <$> loadFontFile path

arialDescriptor :: FontDescriptor
arialDescriptor = FontDescriptor "Arial" $ FontStyle False False

hackDescriptor :: FontDescriptor
hackDescriptor = FontDescriptor "Hack" $ FontStyle False False
