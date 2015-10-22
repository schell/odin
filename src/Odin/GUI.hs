{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Odin.GUI where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Varying
import Control.Arrow (first)
import Data.Renderable
import Odin.Data.Common hiding (Polyline)
import qualified Odin.Data.Common as ODC
import Odin.Graphics.Types
import Gelatin.Core.Rendering (Transform)
import Linear

-- | Some type synonyms.
type Size2 = V2 Float
type Unit = Float
type Coord2 = V2 Float
type Radians = Float
type RadiansDelta = Float

-- | And some new types.

-- | Start angle plus delta radians to turn to reach final angle.
data Angles = Angles Radians RadiansDelta deriving (Show, Eq)
data Texture = Texture deriving (Show, Eq)

-- | Inspired by a language of pictures, from "Composing graphical user
-- interfaces in a purely functional language" Copyright 1998 by Sigbjørn Finne
data PictureCmd f where
    Empty       :: f -> PictureCmd f
    --Point       :: f -> PictureCmd f
    Polyline    :: [Size2] -> f -> PictureCmd f
    Rectangle   :: Size2 -> f -> PictureCmd f
    --Raster      :: Texture -> f -> PictureCmd f
    --Bezier      :: Coord2 -> Coord2 -> Coord2 -> f -> PictureCmd f
    --Ellipse     :: Size2 -> f -> PictureCmd f
    --Circle      :: Unit -> f -> PictureCmd f
    --Arc         :: Size2 -> Angles -> f -> PictureCmd f
    deriving (Show)

instance Functor PictureCmd where
    fmap f (Empty n) = Empty $ f n
    --fmap f (Point n) = Point $ f n
    fmap f (Polyline vs n) = Polyline vs $ f n
    fmap f (Rectangle v n) = Rectangle v $ f n
    --fmap f (Raster t n) = Raster t $ f n
    --fmap f (Bezier a b c n) = Bezier a b c $ f n
    --fmap f (Ellipse v n) = Ellipse v $ f n
    --fmap f (Circle r n) = Circle r $ f n
    --fmap f (Arc v a n) = Arc v a $ f n

type Picture = F PictureCmd

empty :: Picture ()
empty = liftF $ Empty ()

--point :: Picture ()
--point = liftF $ Point ()

line :: Size2 -> Picture ()
line sz = liftF $ Polyline [sz] ()

polyline :: [Size2] -> Picture ()
polyline vs = liftF $ Polyline vs ()

rectangle :: Size2 -> Picture ()
rectangle sz = liftF $ Rectangle sz ()

--raster :: Texture -> Picture ()
--raster tx = liftF $ Raster tx ()
--
--bezier :: Coord2 -> Coord2 -> Coord2 -> Picture ()
--bezier a b c = liftF $ Bezier a b c ()
--
--ellipse :: Size2 -> Picture ()
--ellipse sz = liftF $ Ellipse sz ()
--
--circle :: Unit -> Picture ()
--circle r = liftF $ Circle r ()
--
--arc :: Size2 -> Angles -> Picture ()
--arc sz a = liftF $ Arc sz a ()

-- | Compile the picture commands into a list of renderable primitives.
compilePrimitives :: Free PictureCmd () -> Reader Transform [(Transform, Element IO Rez Transform)]
compilePrimitives (Pure ()) = return []
compilePrimitives (Free (Rectangle sz n)) = do
    t  <- ask
    ps <- compilePrimitives n
    return $ (t, Element $ Box sz $ V4 1 0 0 1) : ps
compilePrimitives (Free (Polyline vs n)) = do
    t <- ask
    ps <- compilePrimitives n
    return $ (t, Element $ ODC.Polyline 2 (V4 0 1 0 1) vs) : ps
compilePrimitives _ = return []

instance Composite (Picture ()) [] IO Rez Transform where
    composite pic = runReader (compilePrimitives $ fromF pic) mempty

-- | Compile the picture commands into a string.
compileString :: Free PictureCmd () -> Reader Int String
compileString (Pure ()) = return ""
compileString (Free (Empty n)) = do
    t <- ask
    s <- compileString n
    return $ replicate t ' ' ++ "Empty\n" ++ s
compileString _ = return ""

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
