module Odin.TH where

lensNames :: [String] -> [(String, String)]
lensNames = map (\s -> (s, s ++ "_"))
