{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module Odin.Core.Physics
  ( module PE
  , External
  , OdinWorld
  , OdinScene
  , World(..)
  , Body(..)
  , emptyBody
  , odinBodyToWorldObj
  , physicalObj
  , worldObject
  , physicalObj2RndTfrms
  , worldObj2RndTfrms
  , worldObjPos
  , worldObjRot
  , applyPhysics
  , emptyScene
  , rectangleHull
  , canonicalizeVec
  , canonicalizePoint
  , canonicalizeConvexHull
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
import           Control.Lens
import           Control.Monad.State.Strict
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

emptyBody :: Body
emptyBody = Body { bVel    = (0, 0)
                 , bRotVel = 0
                 , bPos    = (0, 0)
                 , bRot    = 0
                 , bMass   = (0, 0)
                 , bMu     = 0
                 , bHull   = rectangleHull 0 0
                 }

odinBodyToWorldObj :: Body -> WorldObj
odinBodyToWorldObj Body{..} =
  worldObject bVel bRotVel bPos bRot bMass bMu bHull

emptyScene :: OdinScene
emptyScene = Scene world externals contactBehavior
  where world = emptyWorld
        externals = []
        contactBehavior = ContactBehavior 0.01 0.02

physicalObj2RndTfrms :: PhysicalObj -> [RenderTransform]
physicalObj2RndTfrms PhysicalObj{..} = [v,r]
  where vd = toLV2 _physObjPos
        v  = Spatial $ Translate $ realToFrac <$> vd
        r  = Spatial $ Rotate $ realToFrac _physObjRotPos

worldObj2RndTfrms :: WorldObj -> [RenderTransform]
worldObj2RndTfrms = physicalObj2RndTfrms . _worldPhysObj

applyPhysics :: IntMap WorldObj -> IntMap [RenderTransform]
             -> IntMap [RenderTransform]
applyPhysics objs = IM.intersectionWith (++) objTfrms
  where objTfrms = worldObj2RndTfrms <$> objs
--------------------------------------------------------------------------------
-- Converting Shapes types to Linear types
--------------------------------------------------------------------------------
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
