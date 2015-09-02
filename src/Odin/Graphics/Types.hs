{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Odin.Graphics.Types where

import Linear
import Graphics.UI.GLFW
import Graphics.Text.TrueType
import Gelatin.Core.Rendering
import GHC.Generics (Generic)
--import Control.Concurrent.Async
import Control.Varying
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Fresh
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import Data.Typeable
import Data.Hashable
import Data.Renderable

newtype AttachedRenderings = Attached { attached :: Cache IO Transform }

data Rez = Rez { rezGeom      :: GeomRenderSource
               , rezBez       :: BezRenderSource
               , rezMask      :: MaskRenderSource
               , rezWindow    :: Window
               --, rezFontCache :: Async FontCache
               , rezFont      :: Font
               , rezIcons     :: Font
               } deriving (Typeable)

type MakesScene r = ( ReadsRez r
                    , ModifiesRenderings r
                    , DoesIO r
                    )

type ReadsRez r = Member (Reader Rez) r
type ModifiesTime r = Member (State Delta) r
type DoesIO r = SetMember Lift (Lift IO) r

type ModifiesRenderings r = (Member (State AttachedRenderings) r)

instance Hashable Clip

data Clip = Clip { clipTopLeft     :: V2 Int
                 , clipBottomRight :: V2 Int
                 } deriving (Show, Eq, Typeable, Generic)

instance Hashable Transform
deriving instance Generic Transform
deriving instance Eq Transform

deriving instance Generic PointSize

type Color = V4 Float
type Size = V2 Float
type Position = V2 Float
type Vector = V2 Float
type Scale = V2 Float
type Rotation = Float

newtype Delta = Delta { unDelta :: Double } deriving (Show)


data PicInfo = PicInfo { picInfoWidth  :: Int
                       , picInfoHeight :: Int
                       } deriving (Show, Eq, Ord)

newtype Uid = Uid { unUid :: Int } deriving (Show, Eq, Enum, Ord, Num)

type MakesUid r = Member (Fresh Uid) r

type TimeDelta r = Member (State Delta) r

type Vareff r = Var (Eff r)
