module Main where

import           Control.Monad          (void)
import           Odin.Engine.Eff.Common
import           Odin.Engine.Slots

main :: IO ()
main = void $ runM $ flip runState (Allocated []) $ do
  io $ putStrLn "going to allocate some resources..."
  autoRelease $ do
    s1  <- slot "Some text." $ putStrLn . ("Freeing: " ++)
    s2  <- slot (5 :: Int) $ putStrLn . ("Freeing: " ++) . show
    txt <- unslot s1
    n   <- unslot s2
    mapM_ (io . putStrLn) [txt, show n]
  io $ putStrLn "allocating more resources..."
  autoRelease $ do
    s1 <- slot "More text." $ putStrLn . ("Freeing: " ++)
    s1 $= "Last bit of text."
  io $ putStrLn "Done."
