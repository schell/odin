{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
module Odin.Physics (
  module PE,
  External,
  OdinWorld,
  OdinScene,
  physicalObj,
  worldObject,
  physicalObj2Tfrm,
  physicalObj2PicTfrm,
  worldObj2PicTfrm,
  applyPhysics,
  emptyScene,
) where

import           Physics.Engine                 as PE
import           Physics.Engine.Main            as PE
import           Physics.Constraint             as PE
import           Physics.Contact                as PE
import           Physics.Contact.ConvexHull     as PE
import           Physics.World                  as PE
import           Physics.World.Object           as PE
import           Physics.World.Class (External)
import           Physics.Scenes.Scene           as PE
import           Gelatin.SDL2
import           Data.Monoid ((<>))
import           Data.IntMap.Strict
import qualified Data.IntMap.Strict as IM
import           GHC.Exts

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

physicalObj2Tfrm :: PhysicalObj -> Transform
physicalObj2Tfrm PhysicalObj{..} = Transform v 1 r
  where x# = v2x _physObjPos
        y# = v2y _physObjPos
        vd = V2 (D# x#) (D# y#)
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


