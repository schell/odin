{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Odin.Engine.New.UI.Painting where

import           Gelatin.GL
import           Odin.Engine.New (Shape (..))
import           Reflex.SDL2     hiding (fan)


----------------------------------------------------------------------
-- | A Painting is a Renderer2 with a boundary.
data Painting = Painting { paintingBounds   :: (V2 Float, V2 Float)
                         , paintingRenderer :: Renderer2
                         }


renderPainting :: Painting -> [RenderTransform2] -> IO ()
renderPainting p = snd (paintingRenderer p)


freePainting :: Painting -> IO ()
freePainting = fst . paintingRenderer


paintingSize :: Painting -> V2 Float
paintingSize p = let (tl,br) = paintingBounds p in br - tl


paintingOrigin :: Painting -> V2 Float
paintingOrigin = fst . paintingBounds


paintingCenter :: Painting -> V2 Float
paintingCenter p = let (tl,br) = paintingBounds p in tl + (br - tl)/2


paintingShape :: Painting -> Shape
paintingShape p = let (tl, br) = paintingBounds p
                      V2 w h   = br - tl
                  in ShapeRectangle tl w h


instance Monoid Painting where
  mempty = Painting (0,0) mempty
  mappend (Painting abb a) (Painting bbb b) = Painting cbb c
    where cbb = listToBox [fst abb, snd abb, fst bbb, snd bbb]
          c   = a `mappend` b


----------------------------------------------------------------------
-- | A Painter is a function that creates a Painting.
newtype Painter a m = Painter { runPainter :: a -> m Painting }

paint :: MonadIO m => Painter a IO -> a -> m Painting
paint pnt = liftIO . runPainter pnt
