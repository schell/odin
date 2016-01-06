{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Odin.GUI where

import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Arrow (second)
import Control.Lens hiding (transform)
import Data.Renderable
import Odin.Data.Common
import Gelatin.Core.Rendering hiding (triangulate)
import Gelatin.Core.Rendering.Bezier
import Gelatin.Core.Rendering.Shape as Shape
import Graphics.Text.TrueType
import Linear
--------------------------------------------------------------------------------
-- Stroke
--------------------------------------------------------------------------------
data StrokeAttr = StrokeNone
                | StrokeColor Color
                | StrokeColors [Color]
                | StrokeWidth Float
                | StrokeFeather Float
                | StrokeCaps (LineCap,LineCap)
                deriving (Show)

strokeAttr :: Maybe Stroke -> StrokeAttr -> Maybe Stroke
strokeAttr _ StrokeNone = Nothing
strokeAttr Nothing c = strokeAttr (Just emptyStroke) c
strokeAttr (Just s) (StrokeColor c) = Just $ s & strokeColor .~ c
strokeAttr (Just s) (StrokeColors cs) = Just $ s & strokeColors .~ cs
strokeAttr (Just s) (StrokeWidth w) = Just $ s & strokeWidth .~ w
strokeAttr (Just s) (StrokeFeather t) = Just $ s & strokeFeather .~ t
strokeAttr (Just s) (StrokeCaps cs) = Just $ s & strokeLineCaps .~ cs

--type ControlPoint = V2 Float

--data PathCmd f where
--    CubicCurveTo :: ControlPoint -> ControlPoint -> V2 Float -> f -> PathCmd f
--    CurveTo :: ControlPoint -> V2 Float -> f -> PathCmd f
--    LineTo :: V2 Float -> f -> PathCmd f
--    MoveTo :: V2 Float -> f -> PathCmd f
--    ColorTo :: V4 Float -> f -> PathCmd f
--------------------------------------------------------------------------------
-- Picture
--------------------------------------------------------------------------------
-- | Inspired by a language of pictures, from "Composing graphical user
-- interfaces in a purely functional language" Copyright 1998 by Sigbjørn Finne
data PictureCmd f where
    Blank         :: f -> PictureCmd f
    Polyline      :: [V2 Float] -> f -> PictureCmd f
    Rectangle     :: V2 Float -> f -> PictureCmd f
    Curve         :: V2 Float -> V2 Float -> V2 Float -> f -> PictureCmd f
    --Arc           :: V2 Float -> Float -> Float -> f PictureCmd f
    Ellipse       :: V2 Float -> f -> PictureCmd f
    Circle        :: Float -> f -> PictureCmd f
    Letters       :: Float -> String -> f -> PictureCmd f
    WithStroke    :: [StrokeAttr] -> Picture () -> f -> PictureCmd f
    WithFill      :: Fill -> Picture () -> f -> PictureCmd f
    WithTransform :: Transform -> Picture () -> f -> PictureCmd f
    WithFont      :: Font -> Picture () -> f -> PictureCmd f

instance Functor PictureCmd where
    fmap f (Blank n) = Blank $ f n
    fmap f (Polyline vs n) = Polyline vs $ f n
    fmap f (Rectangle v n) = Rectangle v $ f n
    fmap f (Curve a b c n) = Curve a b c $ f n
    fmap f (Ellipse v n) = Ellipse v $ f n
    fmap f (Circle r n) = Circle r $ f n
    fmap f (Letters px s n) = Letters px s $ f n
    fmap f (WithStroke as p n) = WithStroke as p $ f n
    fmap f (WithFill fill p n) = WithFill fill p $ f n
    fmap f (WithTransform t p n) = WithTransform t p $ f n
    fmap f (WithFont font p n) = WithFont font p $ f n

type Picture = F PictureCmd

instance Monoid (Picture ()) where
    mempty = blank
    mappend = (>>)

data CompileData = CompileData { cdFont :: Maybe Font
                               , cdTransform :: Transform
                               , cdFill :: Fill
                               }

emptyCompileData :: CompileData
emptyCompileData = CompileData Nothing mempty $ FillColor $ const 0
--------------------------------------------------------------------------------
-- Creating Pictures
--------------------------------------------------------------------------------
blank :: Picture ()
blank = liftF $ Blank ()

line :: V2 Float -> Picture ()
line sz = liftF $ Polyline [sz] ()

polyline :: [V2 Float] -> Picture ()
polyline vs = liftF $ Polyline vs ()

rectangle :: V2 Float -> Picture ()
rectangle sz = liftF $ Rectangle sz ()

curve :: V2 Float -> V2 Float -> V2 Float -> Picture ()
curve a b c = liftF $ Curve a b c ()

ellipse :: V2 Float -> Picture ()
ellipse sz = liftF $ Ellipse sz ()

circle :: Float -> Picture ()
circle r = liftF $ Circle r ()

letters :: Float -> String -> Picture ()
letters px s = liftF $ Letters px s ()

withStroke :: [StrokeAttr] -> Picture () -> Picture ()
withStroke attrs pic = liftF $ WithStroke attrs pic ()

withFill :: Fill -> Picture () -> Picture ()
withFill f pic = liftF $ WithFill f pic ()

withTransform :: Transform -> Picture () -> Picture ()
withTransform t pic = liftF $ WithTransform t pic ()

withFont :: Font -> Picture () -> Picture ()
withFont f pic = liftF $ WithFont f pic ()

move :: V2 Float -> Picture () -> Picture ()
move v = withTransform (Transform v 1 0)

scale :: V2 Float -> Picture () -> Picture ()
scale v = withTransform (Transform 0 v 0)

rotate :: Float -> Picture () -> Picture ()
rotate r = withTransform (Transform 0 1 r)
--------------------------------------------------------------------------------
-- Measuring pictures
--------------------------------------------------------------------------------
instance BoundedByBox (Free PictureCmd ()) where
    type BoundingBoxR (Free PictureCmd ()) = CompileData
    type BoundingBox (Free PictureCmd ()) = BBox
    boundingBox _ (Pure ()) = (0,0)
    boundingBox cd (Free (Blank n)) = boundingBox cd n
    boundingBox cd (Free (Polyline vs n)) =
        let t = cdTransform cd
            vs' = transform t vs
        in boundingBox () [boundingBox () vs', boundingBox cd n]
    boundingBox cd (Free (Rectangle v n)) =
        let t = cdTransform cd
            vs = boxPath v
            vs' = transform t vs
        in boundingBox () [boundingBox () vs', boundingBox cd n]
    boundingBox cd (Free (Curve a b c n)) =
        let t = cdTransform cd
            vs = transform t $ subdivideAdaptive 100 0 $ bez3 a b c
        in boundingBox () [boundingBox () vs, boundingBox cd n]
    boundingBox cd (Free (Ellipse (V2 x y) n)) =
        let t = cdTransform cd
            vs = transform t $ bez4sToPath 100 0 $ Shape.ellipse x y
        in boundingBox () [boundingBox () vs, boundingBox cd n]
    boundingBox cd (Free (Circle r n)) =
        let t = cdTransform cd
            vs = transform t $ bez4sToPath 100 0 $ Shape.ellipse r r
        in boundingBox () [boundingBox () vs, boundingBox cd n]
    boundingBox cd (Free (Letters px str n))
        | Just font <- cdFont cd =
            let t = cdTransform cd
                (BoundingBox nx ny xx xy _) = stringBoundingBox font 72 (PointSize px) str
                vs = transform t [V2 nx ny, V2 xx xy]
            in boundingBox () [boundingBox () vs, boundingBox cd n]
        | otherwise = boundingBox cd n
    boundingBox cd (Free (WithStroke _ p n)) =
        boundingBox () [boundingBox cd (fromF p :: Free PictureCmd ()), boundingBox cd n]
    boundingBox cd (Free (WithFill _ p n)) =
        boundingBox () [boundingBox cd (fromF p :: Free PictureCmd ()), boundingBox cd n]
    boundingBox cd (Free (WithTransform t p n)) =
        let t' = cdTransform cd
            cd' = cd{cdTransform = t `mappend` t'}
        in boundingBox () [boundingBox cd' (fromF p :: Free PictureCmd ()), boundingBox cd n]
    boundingBox cd (Free (WithFont font p n)) =
        let cd' = cd{cdFont = Just font}
        in boundingBox () [boundingBox cd' (fromF p :: Free PictureCmd ()), boundingBox cd n]

instance BoundedByBox (Picture ()) where
    type BoundingBoxR (Picture ()) = ()
    type BoundingBox (Picture ()) = BBox
    boundingBox () p = boundingBox emptyCompileData (fromF p :: Free PictureCmd ())

-- Returns the bounding box of the picture.
pictureBounds :: Picture () -> BBox
pictureBounds = boundingBox ()

-- Returns the size of the picuter.
pictureSize :: Picture () -> V2 Float
pictureSize p =
    let (tl,br) = pictureBounds p
    in br - tl

-- Returns the leftmost, uppermost point of the picture.
pictureOrigin :: Picture () -> V2 Float
pictureOrigin = fst . pictureBounds
--------------------------------------------------------------------------------
-- Compiling pictures
--------------------------------------------------------------------------------
-- Returns the next picture command.
nextPicCmd :: Free PictureCmd () -> Free PictureCmd ()
nextPicCmd (Pure ()) = return ()
nextPicCmd (Free (Blank n)) = n
nextPicCmd (Free (Polyline _ n)) = n
nextPicCmd (Free (Rectangle _ n)) = n
nextPicCmd (Free (Curve _ _ _ n)) = n
nextPicCmd (Free (Ellipse _ n)) = n
nextPicCmd (Free (Circle _ n)) = n
nextPicCmd (Free (Letters _ _ n)) = n
nextPicCmd (Free (WithStroke _ _ n)) = n
nextPicCmd (Free (WithFill _ _ n)) = n
nextPicCmd (Free (WithTransform _ _ n)) = n
nextPicCmd (Free (WithFont _ _ n)) = n

-- Compiles a list of paths from a free picture command
compilePaths :: Free PictureCmd () -> Reader CompileData [(Transform, PathPrimitives)]
compilePaths (Pure ()) = return []
compilePaths (Free (Blank n)) = ([] ++) <$> compilePaths n
compilePaths (Free (Polyline vs n)) = do
    t     <- asks cdTransform
    prims <- compilePaths n
    return $ (t, Paths [Path vs]) : prims
compilePaths (Free (Rectangle sz n)) = do
    t     <- asks cdTransform
    prims <- compilePaths n
    let ps  = toPaths (Size sz)
        ps' = map (\p -> (t,Paths [p])) ps
    return $ ps' ++ prims
compilePaths (Free (Curve a b c n)) = do
    t     <- asks cdTransform
    prims <- compilePaths n
    return $ (t, Paths [Path $ subdivideAdaptive 100 0 $ bez3 a b c]) : prims
compilePaths (Free (Ellipse (V2 x y) n)) = do
    t     <- asks cdTransform
    prims <- compilePaths n
    return $ (t, Paths [Path $ bez4sToPath 100 0 $ Shape.ellipse x y]) : prims
compilePaths (Free (Circle rad n)) = do
    t     <- asks cdTransform
    prims <- compilePaths n
    return $ (t, Paths [Path $ bez4sToPath 100 0 $ Shape.ellipse rad rad]) : prims
compilePaths (Free (Letters px s n)) = do
    t     <- asks cdTransform
    mf    <- asks cdFont
    prims <- compilePaths n
    let ps = case mf of
                 Just f -> [(t, PathText f px s)]
                 Nothing -> []
    return $ ps ++ prims
compilePaths (Free (WithStroke _ p n)) = do
    prims <- compilePaths n
    paths <- compilePaths $ fromF p
    return $ paths ++ prims
compilePaths (Free (WithFill _ p n)) = do
    prims <- compilePaths n
    paths <- compilePaths $ fromF p
    return $ paths ++ prims
compilePaths (Free (WithTransform t p n)) = do
    t'  <- asks cdTransform
    ps <- local (\cd -> cd{cdTransform = t' `mappend` t}) (compilePaths $ fromF p)
    (ps ++) <$> compilePaths n
compilePaths (Free (WithFont font p n)) = do
    ps <- local (\cd -> cd{cdFont = Just font}) (compilePaths $ fromF p)
    (ps ++) <$> compilePaths n

-- Compiles a list of fills from a free picture command.
compileFillPrims :: Free PictureCmd ()
                 -> Reader CompileData [(Transform, FillPrimitives)]
compileFillPrims (Pure ()) = return []
compileFillPrims (Free (Blank n)) = ([] ++) <$> compileFillPrims n
compileFillPrims (Free (Polyline vs n)) = do
    t <- asks cdTransform
    f <- asks cdFill
    prims <- compileFillPrims n
    return $ (t, FillPaths f [Path vs]) : prims
compileFillPrims (Free (Rectangle sz n)) = do
    t <- asks cdTransform
    f <- asks cdFill
    prims <- compileFillPrims n
    return $ (t, FillTriangles f $ toTriangles $ Size sz) : prims
compileFillPrims (Free (Curve a b c n)) = do
    t <- asks cdTransform
    f <- asks cdFill
    prims <- compileFillPrims n
    return $ (t, FillBeziers f [bez a b c]) : prims
compileFillPrims (Free (Ellipse (V2 x y) n)) = do
    t <- asks cdTransform
    f <- asks cdFill
    prims <- compileFillPrims n
    return $ (t, FillPaths f [Path $ bez4sToPath 100 0 $ Shape.ellipse x y]) : prims
compileFillPrims (Free (Circle rad n)) = do
    t <- asks cdTransform
    f <- asks cdFill
    prims <- compileFillPrims n
    return $ (t, FillPaths f [Path $ bez4sToPath 100 0 $ Shape.ellipse rad rad]) : prims
compileFillPrims (Free (Letters px s n)) = do
    t  <- asks cdTransform
    f  <- asks cdFill
    mf <- asks cdFont
    prims <- compileFillPrims n
    let ps = case mf of
                 Just fnt -> [(t, FillText f fnt px s)]
                 Nothing  -> []
    return $ ps ++ prims
compileFillPrims (Free (WithStroke _ p n)) =
    (++) <$> compileFillPrims (fromF p) <*> compileFillPrims n
compileFillPrims (Free (WithFill f p n)) = do
    fs <- local (\cd -> cd{cdFill = f}) $ compileFillPrims $ fromF p
    (fs ++) <$> compileFillPrims n
compileFillPrims (Free (WithTransform t p n)) = do
    t'    <- asks cdTransform
    prims <- local (\cd -> cd{cdTransform = t' `mappend` t}) (compileFillPrims $ fromF p)
    (prims ++) <$> compileFillPrims n
compileFillPrims (Free (WithFont font p n)) = do
    ps <- local (\cd -> cd{cdFont = Just font}) $ compileFillPrims $ fromF p
    (ps ++) <$> compileFillPrims n

-- | Compile the picture commands into a list of renderable primitives.
compilePrimitives :: Free PictureCmd ()
                  -> Reader CompileData [(Transform, Element IO Rez Transform)]
compilePrimitives (Pure ()) = return []
compilePrimitives (Free (WithTransform t p n)) = do
    t' <- asks cdTransform
    let f = compilePrimitives $ fromF p
    prims <- local (\cd -> cd{cdTransform = t `mappend` t'}) f
    (prims ++) <$> compilePrimitives n
compilePrimitives (Free (WithStroke attrs p n)) = do
    paths <- compilePaths $ fromF p
    let stroke = foldl strokeAttr Nothing attrs
        prims = case stroke of
                 Nothing -> []
                 Just s  -> map (second (Element . Stroked s)) paths
    (prims ++) <$> compilePrimitives n
compilePrimitives (Free (WithFill fill p n)) = do
    prims <- local (\cd -> cd{cdFill = fill}) $ compileFillPrims $ fromF p
    let prims' = map (second Element) prims
    (prims' ++) <$> compilePrimitives n
compilePrimitives (Free (WithFont font p n)) = do
    prims <- local (\cd -> cd{cdFont = Just font}) $ compilePrimitives $ fromF p
    (prims ++) <$> compilePrimitives n

compilePrimitives p = compilePrimitives $ nextPicCmd p

instance Composite (Picture ()) [] IO Rez Transform where
    composite pic = runReader (compilePrimitives $ fromF pic) cd
        where cd = emptyCompileData
--------------------------------------------------------------------------------
-- Showing a picture as a string
--------------------------------------------------------------------------------
compileLine :: Free PictureCmd () -> String -> Reader Int String
compileLine n s = do
    t <- ask
    let s' = replicate t ' ' ++ s ++ "\n"
    (s' ++) <$> compileString n

-- | Compile the picture commands into a string.
compileString :: Free PictureCmd () -> Reader Int String
compileString (Pure ()) = return ""
compileString (Free (Blank n)) = compileLine n "blank"
compileString (Free (Polyline vs n)) = compileLine n $ "polyline " ++ show vs
compileString (Free (Rectangle sz n)) = compileLine n $ "rectangle " ++ show sz
compileString (Free (Curve a b c n)) = compileLine n $ unwords $ "curve" : map show [a,b,c]
compileString (Free (Ellipse sz n)) = compileLine n $ "ellipse " ++ show sz
compileString (Free (Circle u n)) = compileLine n $ "circle " ++ show u
compileString (Free (Letters px s n)) =
    compileLine n $ unwords ["letters", show px, show s]
compileString (Free (WithStroke attrs p n)) = do
    s <- local (+1) $ compileString $ fromF p
    compileLine n $ "withStroke " ++ show attrs ++ "\n" ++ s
compileString (Free (WithFill f p n)) = do
    s <- local (+1) $ compileString $ fromF p
    compileLine n $ "withFill " ++ show f ++ "\n" ++ s
compileString (Free (WithTransform t p n)) = do
    s <- local (+1) $ compileString $ fromF p
    compileLine n $ "withTransform " ++ show t ++ "\n" ++ s
compileString (Free (WithFont f p n)) = do
    s <- local (+1) $ compileString $ fromF p
    compileLine n $ "withFont " ++ show (descriptorOf f) ++ "\n" ++ s

instance Show (Picture ()) where
    show pic = runReader (compileString $ fromF pic) 0
