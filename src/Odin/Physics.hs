{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
module Odin.Physics (
  module PE,
  External,
  OdinWorld,
  OdinScene,
  OdinObject(..),
  odinObjectToWorldObj,
  physicalObj,
  worldObject,
  physicalObj2Tfrm,
  physicalObj2PicTfrm,
  worldObj2PicTfrm,
  applyPhysics,
  emptyScene,
  rectangleHull,
  canonicalizeVec,
  canonicalizePoint,
  canonicalizeConvexHull,
) where

import           Physics.Engine                 as PE
import           Physics.Engine.Main            as PE
import           Physics.Constraint             as PE
import           Physics.Contact                as PE
import           Physics.Contact.ConvexHull (ConvexHull)
import qualified Physics.Contact.ConvexHull     as PE
import qualified Physics.Contact.SAT            as PE --as O
import           Physics.World                  as PE
import           Physics.World.Object           as PE
import           Physics.World.Class (External)
import           Physics.Scenes.Scene           as PE
import qualified Physics.Linear                 as PE
import           Physics.Linear.Convert         as PE


import           Gelatin.SDL2
import qualified Data.Array as A
import           Data.Monoid ((<>))
import           Data.IntMap.Strict
import qualified Data.IntMap.Strict as IM
import           Linear.Affine (Point(..))
import           GHC.Exts (Double(..))

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

rectangleHull :: Double -> Double -> ConvexHull
rectangleHull (D# w) (D# h) = PE.rectangleHull w h

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
worldObject vel rotvel pos rotpos mass = makeWorldObj phys
  where phys = physicalObj vel rotvel pos rotpos mass

data OdinObject = OdinObject { ooVel    :: (Double, Double)
                             , ooRotVel :: Double
                             , ooPos    :: (Double, Double)
                             , ooRot    :: Double
                             , ooMass   :: (Double, Double)
                             , ooMu     :: Double
                             , ooHull   :: ConvexHull
                             }
odinObjectToWorldObj :: OdinObject -> WorldObj
odinObjectToWorldObj OdinObject{..} =
  worldObject ooVel ooRotVel ooPos ooRot ooMass ooMu ooHull

emptyScene :: OdinScene
emptyScene = Scene world externals contactBehavior
  where world = emptyWorld
        externals = []
        contactBehavior = ContactBehavior 0.01 0.02

physicalObj2Tfrm :: PhysicalObj -> Transform
physicalObj2Tfrm PhysicalObj{..} = Transform v 1 r
  where vd = toLV2 _physObjPos
        v  = realToFrac <$> vd
        r = realToFrac _physObjRotPos

physicalObj2PicTfrm :: PhysicalObj -> PictureTransform
physicalObj2PicTfrm p = PictureTransform t 1 1
  where t = physicalObj2Tfrm p

worldObj2PicTfrm :: WorldObj -> PictureTransform
worldObj2PicTfrm = physicalObj2PicTfrm . _worldPhysObj

applyPhysics :: IntMap WorldObj -> IntMap PictureTransform
             -> IntMap PictureTransform
applyPhysics objs = IM.intersectionWith (<>) objTfrms
  where objTfrms = worldObj2PicTfrm <$> objs
--------------------------------------------------------------------------------
-- Converting Shapes types to Linear types
--------------------------------------------------------------------------------
--instance ToCanonical V2 where
--  type Canonical V2 = V2'
--  toCanonical = toLV2
--
--instance ToCanonical P2 where
--  type Canonical P2 = P2'
--  toCanonical = toLP2
--
--instance ToCanonical O.ContactPoints where
--  type Canonical O.ContactPoints = ContactPoints
--  toCanonical =
--    mapBoth f (fromSP . spMap f)
--    where f = toCanonical . _neighborhoodCenter
--
--instance ToCanonical O.Contact where
--  type Canonical O.Contact = Contact
--  toCanonical O.Contact{..} =
--    Contact
--    (toCanonical _contactPenetrator)
--    (toCanonical . _neighborhoodUnitNormal $ _contactEdge)
--
--instance ToCanonical O.Contact' where
--  type Canonical O.Contact' = Contact
--  toCanonical O.Contact'{..} =
--    Contact
--    (Left . toCanonical $ _contactPenetrator')
--    (toCanonical _contactEdgeNormal')
--
--  type Canonical O.Overlap = Overlap
--  toCanonical O.Overlap{..} =
--    Overlap (e0, e1) depth pen
--    where e0 = toCanonical $ _neighborhoodCenter _overlapEdge
--          e1 = toCanonical . _neighborhoodCenter . _neighborhoodNext $ _overlapEdge
--          n = toCanonical $ _neighborhoodUnitNormal _overlapEdge
--          depth = fmap (*(-_overlapDepth)) n
--          pen = toCanonical $ _neighborhoodCenter _overlapPenetrator

canonicalizePoint :: PE.P2 -> V2 Double
canonicalizePoint = unPoint . toLP2
  where unPoint (P v) = v

canonicalizeVec :: PE.V2 -> V2 Double
canonicalizeVec = toLV2

canonicalizeConvexHull :: ConvexHull -> [V2 Double]
canonicalizeConvexHull = (canonicalizePoint <$>) . A.elems . PE._hullLocalVertices
