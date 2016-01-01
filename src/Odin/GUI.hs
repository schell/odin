{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Odin.GUI where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Varying
import Control.Arrow (first,second)
import Control.Lens hiding (transform)
import Data.Renderable
import Data.Monoid
import Odin.Data.Common
import Gelatin.Core.Rendering hiding (triangulate)
import Gelatin.Core.Rendering.Bezier
import Gelatin.Core.Rendering.Shape as Shape
import Graphics.Text.TrueType
import Linear

-- | Some type synonyms.
type Radians = Float
type RadiansDelta = Float

-- | And some new types.

-- | Start angle plus delta radians to turn to reach final angle.
data Angles = Angles Radians RadiansDelta deriving (Show, Eq)
--------------------------------------------------------------------------------
-- Stroke
--------------------------------------------------------------------------------
data StrokeAttr = StrokeNone
                | StrokeColor Color
                | StrokeWidth Float
                | StrokeFeather Float
                | StrokeCaps (LineCap,LineCap)
                deriving (Show)

strokeAttr :: Maybe Stroke -> StrokeAttr -> Maybe Stroke
strokeAttr _ StrokeNone = Nothing
strokeAttr Nothing c = strokeAttr (Just emptyStroke) c
strokeAttr (Just s) (StrokeColor c) = Just $ s & strokeColor .~ c
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
    Ellipse       :: V2 Float -> f -> PictureCmd f
    Circle        :: Float -> f -> PictureCmd f
    --Arc         :: V2 Float -> Angles -> f -> PictureCmd f
    Letters       :: FontDescriptor -> Float -> String -> f -> PictureCmd f
    WithStroke    :: [StrokeAttr] -> Picture () -> f -> PictureCmd f
    WithFill      :: Fill -> Picture () -> f -> PictureCmd f
    WithTransform :: Transform -> Picture () -> f -> PictureCmd f
    RelativeTo    :: Picture () -> Picture () -> f -> PictureCmd f

instance Functor PictureCmd where
    fmap f (Blank n) = Blank $ f n
    fmap f (Polyline vs n) = Polyline vs $ f n
    fmap f (Rectangle v n) = Rectangle v $ f n
    fmap f (Curve a b c n) = Curve a b c $ f n
    fmap f (Ellipse v n) = Ellipse v $ f n
    fmap f (Circle r n) = Circle r $ f n
    --fmap f (Arc v a n) = Arc v a $ f n
    fmap f (Letters fd px s n) = Letters fd px s $ f n
    fmap f (WithStroke as p n) = WithStroke as p $ f n
    fmap f (WithFill fill p n) = WithFill fill p $ f n
    fmap f (WithTransform t p n) = WithTransform t p $ f n
    fmap f (RelativeTo p1 p2 n) = RelativeTo p1 p2 $ f n

type Picture = F PictureCmd
--------------------------------------------------------------------------------
-- Extracting needed fonts
--------------------------------------------------------------------------------
neededFonts :: Picture () -> [FontDescriptor]
neededFonts = f . fromF
    where f (Pure ()) = []
          f (Free (Letters fd _ _ n)) = fd : f n
          f (Free (WithTransform _ p n)) = neededFonts p ++ f n
          f (Free (WithStroke _ p n)) = neededFonts p ++ f n
          f (Free (WithFill _ p n)) = neededFonts p ++ f n
          f p = f $ nextPicCmd p
--------------------------------------------------------------------------------
-- Picture to Paths
--------------------------------------------------------------------------------
compilePaths :: Free PictureCmd () -> Reader Transform [(Transform, PathPrimitives)]
compilePaths (Pure ()) = return []
compilePaths (Free (Blank n)) = ([] ++) <$> compilePaths n
compilePaths (Free (Polyline vs n)) = do
    t     <- ask
    prims <- compilePaths n
    return $ (t, Paths [Path vs]) : prims
compilePaths (Free (Rectangle sz n)) = do
    t     <- ask
    prims <- compilePaths n
    let ps  = toPaths mempty (Size sz)
        ps' = map (\p -> (t,Paths [p])) ps
    return $ ps' ++ prims
compilePaths (Free (Curve a b c n)) = do
    t     <- ask
    prims <- compilePaths n
    return $ (t, Paths [Path $ subdivideAdaptive 100 0 $ bez3 a b c]) : prims
compilePaths (Free (Ellipse (V2 x y) n)) = do
    t     <- ask
    prims <- compilePaths n
    return $ (t, Paths [Path $ bez4sToPath 100 0 $ Shape.ellipse x y]) : prims
compilePaths (Free (Circle rad n)) = do
    t     <- ask
    prims <- compilePaths n
    return $ (t, Paths [Path $ bez4sToPath 100 0 $ Shape.ellipse rad rad]) : prims
compilePaths (Free (Letters fd px s n)) = do
    t     <- ask
    prims <- compilePaths n
    return $ (t, PathText fd px s) : prims
compilePaths (Free (WithStroke _ p n)) = do
    prims <- compilePaths n
    paths <- compilePaths $ fromF p
    return $ paths ++ prims
compilePaths (Free (WithFill _ p n)) = do
    prims <- compilePaths n
    paths <- compilePaths $ fromF p
    return $ paths ++ prims
compilePaths (Free (WithTransform t p n)) = do
    ps <- local (t <>) (compilePaths $ fromF p)
    (ps ++) <$> compilePaths n
compilePaths (Free (RelativeTo p1 p2 n)) = do
    t  <- ask
    ps <- local (t <>) $ compilePaths $ fromF p1
    let b   = boundsOfPaths ps
        p2' = uncurry adjustBy b p2
    ps' <- compilePaths $ fromF p2'
    (ps' ++) <$> compilePaths n

boundsOfPath :: Transform -> PathPrimitives -> (Float, Float)
boundsOfPath = undefined

boundsOfPaths :: [(Transform, PathPrimitives)] -> (Float,Float)
boundsOfPaths = foldl (\(x,y) (x',y') -> (max x x', max y y')) (0,0) . map (uncurry boundsOfPath)

adjustBy :: Float -> Float -> Picture () -> Picture ()
adjustBy = undefined
--------------------------------------------------------------------------------
-- Filling Pictures
--------------------------------------------------------------------------------
compileFillPrims :: Fill -> Free PictureCmd ()
                 -> Reader Transform [(Transform, FillPrimitives)]
compileFillPrims _ (Pure ()) = return []
compileFillPrims f (Free (Blank n)) = ([] ++) <$> compileFillPrims f n
compileFillPrims f (Free (Polyline vs n)) = do
    t <- ask
    prims <- compileFillPrims f n
    return $ (t, FillPaths f [Path vs]) : prims
compileFillPrims f (Free (Rectangle sz n)) = do
    t <- ask
    prims <- compileFillPrims f n
    return $ (t, FillTriangles f $ toTriangles $ Size sz) : prims
compileFillPrims f (Free (Curve a b c n)) = do
    t <- ask
    prims <- compileFillPrims f n
    return $ (t, FillBeziers f [bez a b c]) : prims
compileFillPrims f (Free (Ellipse (V2 x y) n)) = do
    t <- ask
    prims <- compileFillPrims f n
    return $ (t, FillPaths f [Path $ bez4sToPath 100 0 $ Shape.ellipse x y]) : prims
compileFillPrims f (Free (Circle rad n)) = do
    t <- ask
    prims <- compileFillPrims f n
    return $ (t, FillPaths f [Path $ bez4sToPath 100 0 $ Shape.ellipse rad rad]) : prims
compileFillPrims f (Free (Letters fd px s n)) = do
    t <- ask
    prims <- compileFillPrims f n
    return $ (t, FillText f fd px s) : prims
compileFillPrims f (Free (WithStroke _ p n)) =
    (++) <$> compileFillPrims f (fromF p) <*> compileFillPrims f n
compileFillPrims f (Free (WithFill _ p n)) =
    (++) <$> compileFillPrims f (fromF p) <*> compileFillPrims f n
compileFillPrims f (Free (WithTransform t p n)) = do
    prims <- local (t <>) (compileFillPrims f $ fromF p)
    (prims ++) <$> compileFillPrims f n
compileFillPrims f (Free (RelativeTo p1 p2 n)) = do
    t  <- ask
    ps <- local (t <>) $ compileFillPrims f $ fromF p1
    let b   = boundsOfFills ps
        p2' = uncurry adjustBy b p2
    ps' <- compileFillPrims f $ fromF p2'
    (ps' ++) <$> compileFillPrims f n

boundsOfFill :: Transform -> FillPrimitives -> (Float, Float)
boundsOfFill = undefined

boundsOfFills :: [(Transform, FillPrimitives)] -> (Float,Float)
boundsOfFills = foldl (\(x,y) (x',y') -> (max x x', max y y')) (0,0) . map (uncurry boundsOfFill)
--------------------------------------------------------------------------------
-- Creating Pictures
--------------------------------------------------------------------------------
blank :: Picture ()
blank = liftF $ Blank ()

--point :: Picture ()
--point = liftF $ Point ()

line :: V2 Float -> Picture ()
line sz = liftF $ Polyline [sz] ()

polyline :: [V2 Float] -> Picture ()
polyline vs = liftF $ Polyline vs ()

rectangle :: V2 Float -> Picture ()
rectangle sz = liftF $ Rectangle sz ()

--raster :: Texture -> Picture ()
--raster tx = liftF $ Raster tx ()

curve :: V2 Float -> V2 Float -> V2 Float -> Picture ()
curve a b c = liftF $ Curve a b c ()

ellipse :: V2 Float -> Picture ()
ellipse sz = liftF $ Ellipse sz ()

circle :: Float -> Picture ()
circle r = liftF $ Circle r ()

letters :: FontDescriptor -> Float -> String -> Picture ()
letters fd px s = liftF $ Letters fd px s ()

withStroke :: [StrokeAttr] -> Picture () -> Picture ()
withStroke attrs pic = liftF $ WithStroke attrs pic ()

withFill :: Fill -> Picture () -> Picture ()
withFill f pic = liftF $ WithFill f pic ()

withTransform :: Transform -> Picture () -> Picture ()
withTransform t pic = liftF $ WithTransform t pic ()

--arc :: V2 Float -> Angles -> Picture ()
--arc sz a = liftF $ Arc sz a ()
--

nextPicCmd :: Free PictureCmd () -> Free PictureCmd ()
nextPicCmd (Pure ()) = return ()
nextPicCmd (Free (Blank n)) = n
nextPicCmd (Free (Polyline _ n)) = n
nextPicCmd (Free (Rectangle _ n)) = n
nextPicCmd (Free (Curve _ _ _ n)) = n
nextPicCmd (Free (Ellipse _ n)) = n
nextPicCmd (Free (Circle _ n)) = n
nextPicCmd (Free (Letters _ _ _ n)) = n
nextPicCmd (Free (WithStroke _ _ n)) = n
nextPicCmd (Free (WithFill _ _ n)) = n
nextPicCmd (Free (WithTransform _ _ n)) = n

-- | Compile the picture commands into a list of renderable primitives.
compilePrimitives :: Free PictureCmd ()
                  -> Reader Transform [(Transform, Element IO Rez Transform)]
compilePrimitives (Pure ()) = return []
compilePrimitives (Free (WithTransform t p n)) = do
    prims <- local (t <>) $ compilePrimitives $ fromF p
    (prims ++) <$> compilePrimitives n
compilePrimitives (Free (WithStroke attrs p n)) = do
    paths <- compilePaths $ fromF p
    let stroke = foldl strokeAttr Nothing attrs
        prims = case stroke of
                 Nothing -> []
                 Just s  -> map (second (Element . Stroked s)) paths
    (prims ++) <$> compilePrimitives n
compilePrimitives (Free (WithFill fill p n)) = do
    prims <- compileFillPrims fill $ fromF p
    let prims' = map (second Element) prims
    (prims' ++) <$> compilePrimitives n
compilePrimitives p = compilePrimitives $ nextPicCmd p

instance Composite (Picture ()) [] IO Rez Transform where
    composite pic = runReader (compilePrimitives $ fromF pic) mempty
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
compileString (Free (Letters fd px s n)) =
    compileLine n $ unwords ["letters", show fd, show px, show s]
compileString (Free (WithStroke attrs p n)) = do
    s <- local (+1) $ compileString $ fromF p
    compileLine n $ "withStroke " ++ show attrs ++ "\n" ++ s
compileString (Free (WithFill f p n)) = do
    s <- local (+1) $ compileString $ fromF p
    compileLine n $ "withFill " ++ show f ++ "\n" ++ s
compileString (Free (WithTransform t p n)) = do
    s <- local (+1) $ compileString $ fromF p
    compileLine n $ "withTransform " ++ show t ++ "\n" ++ s

instance Show (Picture ()) where
    show pic = runReader (compileString $ fromF pic) 0

--------------------------------------------------------------------------------
-- $creation
-- In order to create a spline you must first have a datatype with a 'Composite'
-- instance. Then you must create a 'varying' value of that datatype
-- describing how it will change over the domain of user input and time.
-- Also needed is an event stream that eventually ends the user's interaction.
-- The idea is that your interface varies over time and user input but
-- eventually produces a result value that can be used in a monadic
-- sequence.
--------------------------------------------------------------------------------
-- | Create a spline describing an interface that eventually produces a value.
-- The type used to represent the user interface must have a 'Composite'
-- instance. This allows GUIs to be layered graphically since separate
-- GUI's iteration values can all be combined after being broken down into
-- transformed primitives.
gui :: (Monad m, Monad n, Monoid (f (t, Element n r t)), Composite a f n r t)
    => Var m i a
    -- ^ The stream of a changing user interface.
    -> Var m i (Event b)
    -- ^ The event stream that concludes a user\'s interaction. When this
    -- stream produces an event the interaction will end and the spline
    -- will conclude.
    -> SplineT f i (t, Element n r t) m (a,b)
gui v ve = SplineT $ t ~> var (uncurry f)
    where t = (,) <$> v <*> ve
          f a e = let ui = composite a
                  in case e of
                         NoEvent -> Step ui NoEvent
                         Event b -> Step ui $ Event (a, b)

wait :: (Monad m, Monad n, Monoid (f (t, Element n r t)), Composite a f n r t)
     => Int -> a -> SplineT f i (t, Element n r t) m ()
wait n b = void $ gui (pure b) $ dropE n $ always ()

--------------------------------------------------------------------------------
-- $transformation
-- Simply put - here we are applying some kind of transformation to your
-- renderable interface. This most likely a standard two or three dimensional
-- affine transformation. Since the transformation also changes over the
-- same domain it\'s possible to tween GUIs.
--------------------------------------------------------------------------------
-- | Transforms a GUI.
transformGUI :: (Monad m, Monoid t, Functor f, Monoid (f (t,d)))
             => Var m i t -> SplineT f i (t, d) m b -> SplineT f i (t, d) m b
transformGUI vt = mapOutput $ vt ~> var f
    where f t = fmap (first (mappend t))
