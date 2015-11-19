{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Odin.GUI where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Varying
import Control.Arrow (first)
import Control.Lens hiding (transform)
import Data.Renderable
import Data.Monoid
import Odin.Data.Common hiding (Polyline)
import qualified Odin.Data.Common as ODC
import Odin.Graphics.Types
import Gelatin.Core.Rendering hiding (triangulate)
import Gelatin.Core.Rendering.Bezier
import Gelatin.Core.Rendering.Shape as Shape
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
    --Point         :: f -> PictureCmd f
    Polyline      :: [V2 Float] -> f -> PictureCmd f
    Rectangle     :: V2 Float -> f -> PictureCmd f
    Curve         :: V2 Float -> V2 Float -> V2 Float -> f -> PictureCmd f
    Ellipse       :: V2 Float -> f -> PictureCmd f
    Circle        :: Float -> f -> PictureCmd f
    --Arc         :: V2 Float -> Angles -> f -> PictureCmd f
    WithStroke    :: [StrokeAttr] -> Picture () -> f -> PictureCmd f
    WithFill      :: Fill -> Picture () -> f -> PictureCmd f
    WithTransform :: Transform -> Picture () -> f -> PictureCmd f
    deriving (Show)

instance Functor PictureCmd where
    fmap f (Blank n) = Blank $ f n
    fmap f (WithStroke as p n) = WithStroke as p $ f n
    fmap f (WithFill fill p n) = WithFill fill p $ f n
    fmap f (WithTransform t p n) = WithTransform t p $ f n
    --fmap f (Point n) = Point $ f n
    fmap f (Polyline vs n) = Polyline vs $ f n
    fmap f (Rectangle v n) = Rectangle v $ f n
    fmap f (Curve a b c n) = Curve a b c $ f n
    fmap f (Ellipse v n) = Ellipse v $ f n
    fmap f (Circle r n) = Circle r $ f n
    --fmap f (Arc v a n) = Arc v a $ f n

type Picture = F PictureCmd

--------------------------------------------------------------------------------
-- Picture to Paths
--------------------------------------------------------------------------------
compilePaths :: Free PictureCmd () -> [Path]
compilePaths (Pure ()) = []
compilePaths (Free (Blank n)) = [] ++ compilePaths n
compilePaths (Free (Polyline vs n)) = [Path vs] ++ compilePaths n
compilePaths (Free (Rectangle sz n)) = toPaths (Size sz) ++ compilePaths n
compilePaths (Free (Curve a b c n)) =
    [Path $ subdivideAdaptive 100 0 $ bez3 a b c] ++ compilePaths n
compilePaths (Free (Ellipse (V2 x y) n)) =
    [Path $ bez4sToPath 100 0 $ Shape.ellipse x y] ++ compilePaths n
compilePaths (Free (Circle r n)) =
    [Path $ bez4sToPath 100 0 $ Shape.ellipse r r] ++ compilePaths n
compilePaths (Free (WithStroke _ p n)) =
    compilePaths (fromF p) ++ compilePaths n
compilePaths (Free (WithFill _ p n)) = compilePaths (fromF p) ++ compilePaths n
compilePaths (Free (WithTransform t p n)) =
    (transform t (compilePaths $ fromF p)) ++ compilePaths n

instance ToPaths (Picture ()) where
    toPaths = compilePaths . fromF
--------------------------------------------------------------------------------
-- Filling Pictures
--------------------------------------------------------------------------------
compileFillPrims :: Fill -> Free PictureCmd () -> [FillPrimitives]
compileFillPrims _ (Pure ()) = []
compileFillPrims f (Free (Blank n)) = [] ++ compileFillPrims f n
compileFillPrims f (Free (Polyline vs n)) =
    FillPaths f [Path vs] : compileFillPrims f n
compileFillPrims f (Free (Rectangle sz n)) =
    FillTriangles f (toTriangles (Size sz)) : compileFillPrims f n
compileFillPrims f (Free (Curve a b c n)) =
    FillBeziers f [bez a b c] : compileFillPrims f n
compileFillPrims f (Free (Ellipse (V2 x y) n)) =
    FillPaths f [Path $ bez4sToPath 100 0 $ Shape.ellipse x y] : compileFillPrims f n
compileFillPrims f (Free (Circle r n)) =
    FillPaths f [Path $ bez4sToPath 100 0 $ Shape.ellipse r r] : compileFillPrims f n
compileFillPrims f (Free (WithStroke _ p n)) =
    compileFillPrims f (fromF p) ++ compileFillPrims f n
compileFillPrims f (Free (WithFill _ p n)) =
    compileFillPrims f (fromF p) ++ compileFillPrims f n
compileFillPrims f (Free (WithTransform t p n)) =
    transform t (compileFillPrims f $ fromF p) ++ compileFillPrims f n
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
nextPicCmd (Free (WithStroke _ _ n)) = n
nextPicCmd (Free (WithFill _ _ n)) = n
nextPicCmd (Free (WithTransform _ _ n)) = n

-- | Compile the picture commands into a list of renderable primitives.
compilePrimitives :: Free PictureCmd () -> Reader Transform [(Transform, Element IO Rez Transform)]
compilePrimitives (Pure ()) = return []
compilePrimitives (Free (WithTransform t p n)) = do
    prims <- local (t <>) $ compilePrimitives $ fromF p
    (prims ++) <$> compilePrimitives n
compilePrimitives (Free (WithStroke attrs p n)) = do
    t <- ask
    let paths = toPaths p
        stroke = foldl strokeAttr Nothing attrs
        prims = case stroke of
                 Nothing -> []
                 Just s  -> map (\path -> (t, Element $ Stroked s path)) paths
    (prims ++) <$> compilePrimitives n
compilePrimitives (Free (WithFill fill p n)) = do
    t <- ask
    let prims = map (\prim -> (t, Element prim)) $ compileFillPrims fill $ fromF p
    (prims ++) <$> compilePrimitives n

compilePrimitives p = compilePrimitives $ nextPicCmd p

instance Composite (Picture ()) [] IO Rez Transform where
    composite pic = runReader (compilePrimitives $ fromF pic) mempty

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
