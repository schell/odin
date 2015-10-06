-- |
--   Module:     Main
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--   The entrypoint to our nifty editor.
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Odin.Control
import Odin.System.Workspace

main :: IO ()
main = runWorkspace network
