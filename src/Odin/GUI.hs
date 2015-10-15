module Odin.GUI where

import Control.Varying
import Control.Arrow (first)
import Data.Renderable
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
