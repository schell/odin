{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns     #-}
module Odin.Engine.New.UI.Animation where

import           Control.Varying
import           Data.Functor.Identity      (Identity (..))
import           Reflex.SDL2

import           Odin.Engine.New
import           Odin.Engine.New.UI.Configs


runAnime :: Var Float b -> Float -> (b, Var Float b)
runAnime v = runIdentity . runVarT v


anime :: Odin r t m => Var Float b -> AnimeCfg t -> m (Dynamic t b)
anime v cfg = do
 dbAndV <- foldDyn (\t (_, vn) -> runAnime vn t) (runAnime v 0) (cfg ^. deltaSecondsEvent)
 return $ fst <$> dbAndV
