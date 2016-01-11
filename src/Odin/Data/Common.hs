{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Odin.Data.Common where

import Graphics.GL.Core33
import Graphics.UI.GLFW
import Graphics.Text.TrueType hiding (BoundingBox)
import Gelatin.Core
import Gelatin.GLFW
import Linear hiding (mult)
import Data.Typeable
import Data.Hashable
import Data.IORef
import Data.Renderable
import Data.Monoid
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import Control.Lens.TH
import Control.Varying
import Control.Monad
import Control.Monad.Trans.RWS.Strict
import Control.Concurrent.Async

newtype AttachedRenderings = Attached { attached :: Cache IO Transform }


data Clip = Clip { clipTopLeft     :: V2 Int
                 , clipBottomRight :: V2 Int
                 } deriving (Show, Eq, Typeable, Generic)

instance Hashable Transform
deriving instance Generic Transform
deriving instance Eq Transform

deriving instance Generic PointSize

type Color = V4 Float
type Position = V2 Float
type Vector = V2 Float
type Scale = V2 Float
type Rotation = Float

newtype Delta = Delta { unDelta :: Double } deriving (Show)


data PicInfo = PicInfo { picInfoWidth  :: Int
                       , picInfoHeight :: Int
                       } deriving (Show, Eq, Ord)

newtype Uid = Uid { unUid :: Int } deriving (Show, Eq, Enum, Ord, Num)
--------------------------------------------------------------------------------
-- User Input
--------------------------------------------------------------------------------
data KeyInput = KeyChar Char
              | KeyMod Key
              deriving (Show, Eq, Typeable)

data InputEvent = NoInputEvent
                | CharEvent Char
                | WindowSizeEvent Int Int
                | KeyEvent Key Int KeyState ModifierKeys
                -- ^ Key, scancode, pressed/released, mods
                | MouseButtonEvent MouseButton MouseButtonState ModifierKeys
                | CursorMoveEvent Double Double
                | CursorEnterEvent CursorState
                | ScrollEvent Double Double
                | FileDropEvent [String]
                deriving (Show, Eq, Ord)

instance Monoid InputEvent where
    mempty = NoInputEvent
    mappend NoInputEvent e = e
    mappend e _ = e
--------------------------------------------------------------------------------
-- Really simple stuff
--------------------------------------------------------------------------------
fromPrim :: (Primitive a, Hashable a, Monad (PrimM a), Monoid (PrimT a))
         => a -> (PrimT a, Element (PrimM a) (PrimR a) (PrimT a))
fromPrim a = (mempty, Element a)
--------------------------------------------------------------------------------
-- Low level infrastructure
--------------------------------------------------------------------------------
-- | ReadData is a type that stores resources and certain values pertaining to
-- user input. Without storing things like the last cursor position our varying
-- values have to wait for an event before taking a real value, which means
-- placeholder values must be used until such an event. Many times using a
-- placeholder value results in a signal that is less than ideal. By
-- storing certain important user input events in a read-only structure we can
-- query the event to construct a nice signal, e.g. a signal that would rely on
-- cursor position can have a (usable) value instantly.
data ReadData = ReadData { _readCursorPos :: V2 Float
                         , _readWindowSize :: V2 Float
                         , _readResources :: Rez
                         , _readDpi :: Dpi
                         }
makeLenses ''ReadData

type ControlM = RWST ReadData () () IO
type Odin = SplineT [] InputEvent Component ControlM
type Varying = Var ControlM InputEvent
type VaryingOn = Var ControlM
type SplineOf a b c = Spline a b ControlM c

type Component = (Transform, Element IO Rez Transform)
type UI = [Component]

instance Primitive () where
    type PrimM () = IO
    type PrimR  () = Rez
    type PrimT  () = Transform
    compilePrimitive _ _ = return (return (), const $ return ())

instance Composite () [] IO Rez Transform where
    composite () = []
--------------------------------------------------------------------------------
-- Measuring things with boundaries
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Path
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Box
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Picture Helpers
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Decomposing things into paths
--------------------------------------------------------------------------------

--instance BoundedByBox PathPrimitives where
--    type BoundingBoxR PathPrimitives = FontMap
--    type BoundingBox PathPrimitives = (V2 Float, V2 Float)
--    boundingBox m prims = boundingBox () $ concatMap unPath $ toPaths m prims




--instance ToPaths a => BoundedByBox (Stroked a) where
--    type BoundingBoxR (Stroked a) = FontMap
--    type BoundingBox (Stroked a) = (V2 Float, V2 Float)
--    boundingBox m (Stroked (Stroke _ w _ _) p) = (tl - v, br + v)
--        where v = 0.5 ^* w
--              (tl,br) = boundingBox () $ concatMap unPath $ toPaths m p

--------------------------------------------------------------------------------
-- Filling things
--------------------------------------------------------------------------------
--instance BoundedByBox FillPrimitives where
--    type BoundingBoxR FillPrimitives = FontMap
--    type BoundingBox FillPrimitives = BBox
--    boundingBox m (FillText _ fd w s) = boundingBox m $ PathText fd w s
--    boundingBox _ prims = boundingBox () $ concat $ fillPrimsPoints prims

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Workspace
--------------------------------------------------------------------------------
-- | A 'Workspace' is the largest concept in Odin. One Workspace is
-- rendered to one window.
data Workspace = Workspace { wsFile  :: Maybe FilePath
                           -- ^ The save location of the workspace.
                           , wsRez   :: Rez
                           -- ^ The rendering resources for the workspace
                           , wsCache :: Cache IO Transform
                           -- ^ The rendering cache for the workspace
                           , wsRef   :: IORef [InputEvent]
                           -- ^ The input ioref
                           , wsRead  :: ReadData
                           -- ^ The readable data for our control structures
                           }
