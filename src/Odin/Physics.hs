module Odin.Physics (
  module PE,
  External,
  OdinWorld,
  OdinScene,
  physicalObj,
  worldObject,
  emptyScene
) where

import Physics.Engine                 as PE
import Physics.Engine.Main            as PE
import Physics.Constraint             as PE
import Physics.Contact                as PE
import Physics.Contact.ConvexHull     as PE
import Physics.World                  as PE
import Physics.World.Object           as PE
import Physics.World.Class (External)
import Physics.Scenes.Scene           as PE

type OdinWorld = World WorldObj
type OdinScene = Scene Engine

physicalObj :: (Double, Double)
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
physicalObj vel rotvel pos rotpos =
  PhysicalObj (pairToV2 vel) rotvel (pairToV2 pos) rotpos . toInvMass2

worldObject :: (Double, Double)
               -- ^ The velocity of the object.
            -> Double
               -- ^ The rotational velocity of the object.
            -> (Double, Double)
               -- ^ The position of the object.
            -> Double
               -- ^ The rotation of the object.
            -> (Double, Double)
               -- ^ Linear and rotation mass of the object.
            -> Double
               -- ^ Mu?
            -> ConvexHull
               -- ^ The shape of the object.
            -> WorldObj
worldObject vel rotvel pos rotpos mass mu shape = makeWorldObj phys mu shape
  where phys = physicalObj vel rotvel pos rotpos mass

emptyScene :: OdinScene
emptyScene = Scene world externals contactBehavior
  where world = emptyWorld
        externals = []
        contactBehavior = ContactBehavior 0.01 0.02
