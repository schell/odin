{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Odin.Graphics.Renderable (
    UI,
    Box(..),
    PlainText(..),
    Icon(..)
) where

import Odin.Graphics.Types
import Graphics.GL.Core33
import Graphics.UI.GLFW
import Graphics.Text.TrueType
import Gelatin.Core.Rendering as G
import Gelatin.Core.Color
import Linear hiding (mult)
import Data.Typeable
import Data.Hashable
import GHC.Generics (Generic)
import Data.Renderable as R
import Data.IntMap as IM
--------------------------------------------------------------------------------
-- Icon
--------------------------------------------------------------------------------
instance Renderable Icon where
    type RenderMonad Icon = IO
    type RenderRsrc Icon = Rez
    type RenderTfrm Icon = Transform
    nameOf _ = "Icon"
    cache (Rez geom bz _ win _ font) rs p@(Icon _ str fc) = do
        r <- gtor <$> stringRendering win geom bz font str fc
        return $ IM.insert (hash p) r rs
    composite p = [(hash p, Just $ iconTfrm p)]

instance Hashable Icon where
    hashWithSalt s (Icon _ t c) = s `hashWithSalt` t `hashWithSalt` c

data Icon = Icon { iconTfrm  :: Transform
                 , iconString :: String
                 , iconColor  :: Color
                 } deriving (Show, Eq)
--------------------------------------------------------------------------------
-- PlainText
--------------------------------------------------------------------------------
instance Renderable PlainText where
    type RenderMonad PlainText = IO
    type RenderRsrc PlainText = Rez
    type RenderTfrm PlainText = Transform
    nameOf _ = "PlainText"
    cache (Rez geom bz _ win font _) rs p@(PlainText _ str fc) = do
        r <- gtor <$> stringRendering win geom bz font str fc
        return $ IM.insert (hash p) r rs
    composite p = [(hash p, Just $ plainTextTfrm p)]

instance Hashable PlainText where
    hashWithSalt s (PlainText _ t c) = s `hashWithSalt` t `hashWithSalt` c

data PlainText = PlainText { plainTextTfrm  :: Transform
                           , plainTxtString :: String
                           , plainTxtColor  :: Color
                           } deriving (Show, Eq)
--------------------------------------------------------------------------------
-- Box
--------------------------------------------------------------------------------
instance Renderable Box where
    type RenderMonad Box = IO
    type RenderRsrc Box  = Rez
    type RenderTfrm Box  = Transform
    nameOf _ = "Box"
    cache (Rez geom _ _ win _ _) rs b@(Box _ (V2 w h) c) = do
        let [tl, tr, br, bl] = [zero, V2 w 0, V2 w h, V2 0 h]
            vs = [tl, tr, br, tl, br, bl]
            cs = replicate 6 c
        G.Rendering f c' <- colorRendering win geom GL_TRIANGLES vs cs
        putStrLn $ "Cacheing Box " ++ (show $ hash b)
        return $ IM.insert (hash b) (R.Rendering f c') rs
    composite b = [(hash b, Just $ boxTransform b)]

instance Hashable Box where
    hashWithSalt s (Box _ sz c) = s `hashWithSalt` sz `hashWithSalt` c

data Box = Box { boxTransform :: Transform
               , boxSize      :: Size
               , boxColor     :: Color
               } deriving (Show, Eq, Typeable, Generic)

type UI = Element IO Rez Transform
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
gtor :: G.Rendering -> R.Rendering IO Transform
gtor (G.Rendering f c) = R.Rendering f c

stringRendering :: Window -> GeomRenderSource -> BezRenderSource -> Font
                -> String -> Color -> IO G.Rendering
stringRendering win geom bz font str fc = do
    -- Some docs
    let mult = 2 :: Float
        movv = 1/4 :: Double
        movh = 1/2 :: Double
        fstr = FontString font (mult*16) (0,0) str
        fc'  = fc `alpha` 0.25
        t    = Transform 0 (V2 (1/mult) (1/mult)) 0
    G.Rendering r1 c1 <- colorFontRendering win geom bz fstr $ const fc'
    G.Rendering r2 c2 <- colorFontRendering win geom bz fstr $ const fc
    let f t' = do r1 (translate (-movh) 0 t')
                  r1 (translate movh 0 t')
                  r1 (translate 0 movv t')
                  r1 (translate 0 (-movv) t')
                  r2 t'
    return $ transformRendering t $ G.Rendering f (c1 >> c2)


