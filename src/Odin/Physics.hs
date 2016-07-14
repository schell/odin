module Odin.Physics where

import Physics.Engine
import Physics.Constraint
import Physics.World
import Physics.Scenes.Scene

import Odin.Common

type OdinWorld = World Entity
type OdinScene = Scene Engine

makeObj :: (Double, Double)
           -- ^ The velocity of the object.
        -> Double
           -- ^ The rotational velocity of the object.
        -> (Double, Double)
           -- ^ The position of the object.
        -> Double
           -- ^ The rotation of the object.
        -> (Double, Double)
           -- ^ Mass
        -> PhysicalObj
makeObj vel rotvel pos rotpos =
  PhysicalObj (pairToV2 vel) rotvel (pairToV2 pos) rotpos . toInvMass2

boxA :: PhysicalObj
boxA = makeObj (1,0) 0 (-5,0) 0 (2,1)
