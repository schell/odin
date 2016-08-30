{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
module Odin.Core.Physics (
  module PE,
  External,
  OdinWorld,
  OdinScene,
  World(..),
  Body(..),
  odinBodyToWorldObj,
  physicalObj,
  worldObject,
  physicalObj2PicTfrm,
  worldObj2PicTfrm,
  applyPhysics,
  emptyScene,
  rectangleHull,
  canonicalizeVec,
  canonicalizePoint,
  canonicalizeConvexHull,
  worldObjPos,
  worldObjRot
) where

import           Physics.Engine                 as PE
import           Physics.Engine.Main            as PE
import           Physics.Constraint             as PE
import           Physics.Contact                as PE
import           Physics.Contact.ConvexHull (ConvexHull)
import qualified Physics.Contact.ConvexHull     as PE
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

data Body = Body { bVel    :: (Double, Double)
                 , bRotVel :: Double
                 , bPos    :: (Double, Double)
                 , bRot    :: Double
                 , bMass   :: (Double, Double)
                 , bMu     :: Double
                 , bHull   :: ConvexHull
                 }

odinBodyToWorldObj :: Body -> WorldObj
odinBodyToWorldObj Body{..} =
  worldObject bVel bRotVel bPos bRot bMass bMu bHull

emptyScene :: OdinScene
emptyScene = Scene world externals contactBehavior
  where world = emptyWorld
        externals = []
        contactBehavior = ContactBehavior 0.01 0.02

physicalObj2PicTfrm :: PhysicalObj -> PictureTransform
physicalObj2PicTfrm PhysicalObj{..} = PictureTransform mv 1 1 Nothing
  where vd = toLV2 _physObjPos
        v  = mat4Translate $ promoteV2 (realToFrac <$> vd)
        r = mat4Rotate (realToFrac _physObjRotPos) (V3 0 0 1)
        mv = v !*! r

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
--    where f = toCanonical . _neighborhbdCenter
--
--instance ToCanonical O.Contact where
--  type Canonical O.Contact = Contact
--  toCanonical O.Contact{..} =
--    Contact
--    (toCanonical _contactPenetrator)
--    (toCanonical . _neighborhbdUnitNormal $ _contactEdge)
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
--    where e0 = toCanonical $ _neighborhbdCenter _overlapEdge
--          e1 = toCanonical . _neighborhbdCenter . _neighborhbdNext $ _overlapEdge
--          n = toCanonical $ _neighborhbdUnitNormal _overlapEdge
--          depth = fmap (*(-_overlapDepth)) n
--          pen = toCanonical $ _neighborhbdCenter _overlapPenetrator

canonicalizePoint :: PE.P2 -> V2 Double
canonicalizePoint = unPoint . toLP2
  where unPoint (P v) = v

canonicalizeVec :: PE.V2 -> V2 Double
canonicalizeVec = toLV2

canonicalizeConvexHull :: ConvexHull -> [V2 Double]
canonicalizeConvexHull = (canonicalizePoint <$>) . A.elems . PE._hullLocalVertices

worldObjPos :: WorldObj -> V2 Float
worldObjPos = (realToFrac <$>) . canonicalizeVec . _physObjPos . _worldPhysObj

worldObjRot :: WorldObj -> Float
worldObjRot = realToFrac . _physObjRotPos . _worldPhysObj
