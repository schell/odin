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
import Gelatin.Core.Rendering as G
import Gelatin.Core.Rendering.Bezier
import Gelatin.Core.Color
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

type FontMap = Map FontDescriptor Font

data Rez = Rez { rezShader :: SumShader
               , rezWindow :: Window
               , rezFontCache :: Async FontCache
               , rezFonts  :: FontMap
               } deriving (Typeable)

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

data Request = RequestFont FontDescriptor
             deriving (Show)

type ControlM = RWST ReadData [Request] () IO
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
class BoundedByBox a where
    type BoundingBoxR a :: *
    type BoundingBox a :: *
    boundingBox :: BoundingBoxR a -> a -> BoundingBox a

type BBox = (V2 Float, V2 Float)

instance BoundedByBox [V2 Float] where
    type BoundingBoxR [V2 Float] = ()
    type BoundingBox [V2 Float] = (V2 Float, V2 Float)
    boundingBox _ [] = (0,0)
    boundingBox _ ps = (V2 minx miny, V2 maxx maxy)
        where xs = map fx ps
              ys = map fy ps
              minx = minimum xs
              miny = minimum ys
              maxx = maximum xs
              maxy = maximum ys
              fx (V2 x _) = x
              fy (V2 _ y) = y

instance BoundedByBox [BBox] where
    type BoundingBoxR [BBox] = ()
    type BoundingBox [BBox] = BBox
    boundingBox _ [] = (0,0)
    boundingBox _ bs = boundingBox () ps
        where ps = concatMap (\(v1,v2) -> [v1,v2]) bs

instance BoundedByBox (Triangle (V2 Float)) where
    type BoundingBoxR (Triangle (V2 Float)) = ()
    type BoundingBox (Triangle (V2 Float)) = BBox
    boundingBox _ (Triangle a b c) = boundingBox () [a,b,c]
--------------------------------------------------------------------------------
-- Path
--------------------------------------------------------------------------------
newtype Path = Path { unPath :: [V2 Float] } deriving (Show, Generic)

instance Transformable Transform Path where
    transform t (Path vs) = Path $ transform t vs

instance Hashable Path

instance BoundedByBox Path where
    type BoundingBoxR Path = ()
    type BoundingBox Path = (V2 Float, V2 Float)
    boundingBox _ (Path ps) = boundingBox () ps

data PathPrimitives = Paths [Path]
                    | PathText Font Float String
                    deriving (Generic, Show)

instance Hashable Font where
    hashWithSalt s _ = s `hashWithSalt` "Font"

instance Hashable PathPrimitives
--------------------------------------------------------------------------------
-- Box
--------------------------------------------------------------------------------
boxPath :: V2 Float -> Path
boxPath (V2 w h) = Path poly
    where poly = [V2 x1 y1, V2 x2 y1, V2 x2 y2, V2 x1 y2, V2 x1 y1]
          x1 = 0
          x2 = w
          y1 = 0
          y2 = h
--------------------------------------------------------------------------------
-- Picture Helpers
--------------------------------------------------------------------------------
newtype Size = Size (V2 Float)
--------------------------------------------------------------------------------
-- Decomposing things into paths
--------------------------------------------------------------------------------
class ToPaths a where
    toPaths :: a -> [Path]

instance ToPaths [V2 Float] where
    toPaths vs = [Path vs]

instance ToPaths Size where
    toPaths (Size sz) = [boxPath sz]

instance ToPaths Path where
    toPaths p = [p]

instance ToPaths PathPrimitives where
    toPaths (Paths ps) = ps
    toPaths (PathText f px str) =
        let qs = fontCurves 72 f px str
            sub = subdivideAdaptive 100 0
            mkPath = Path . cleanSeqDupes . concat . fmap sub
            in concat $ fmap (fmap mkPath) qs

--instance BoundedByBox PathPrimitives where
--    type BoundingBoxR PathPrimitives = FontMap
--    type BoundingBox PathPrimitives = (V2 Float, V2 Float)
--    boundingBox m prims = boundingBox () $ concatMap unPath $ toPaths m prims
--------------------------------------------------------------------------------
-- Stroke
--------------------------------------------------------------------------------
data Stroke = Stroke { _strokeColor   :: Color
                     , _strokeWidth   :: Float
                     , _strokeFeather :: Float
                     , _strokeLineCaps:: (LineCap,LineCap)
                     } deriving (Show, Generic)
makeLenses ''Stroke

emptyStroke :: Stroke
emptyStroke = Stroke 0 2 1 (LineCapRound,LineCapRound)

data Stroked a = Stroked Stroke a deriving (Show, Generic)

deriving instance Generic LineCap
instance Hashable LineCap
instance Hashable Stroke
instance Hashable a => Hashable (Stroked a)

instance ToPaths a => Primitive (Stroked a) where
    type PrimM (Stroked a) = IO
    type PrimR (Stroked a) = Rez
    type PrimT (Stroked a) = Transform
    canAllocPrimitive (Rez _ _ _ _) (Stroked _ p) = not $ null $ toPaths p
    compilePrimitive (Rez sh win _ _) (Stroked (Stroke c w f cp) p) = do
        let ps = toPaths p
            cs = repeat c
            shader = _shProjectedPolyline sh
        rs <- forM ps $ \(Path vs) ->
            projectedPolylineRendering win shader w f cp vs cs
        let Rendering a b = foldl (<>) mempty rs
        return (b, a)

--instance ToPaths a => BoundedByBox (Stroked a) where
--    type BoundingBoxR (Stroked a) = FontMap
--    type BoundingBox (Stroked a) = (V2 Float, V2 Float)
--    boundingBox m (Stroked (Stroke _ w _ _) p) = (tl - v, br + v)
--        where v = 0.5 ^* w
--              (tl,br) = boundingBox () $ concatMap unPath $ toPaths m p
--------------------------------------------------------------------------------
-- Decomposing things into triangles
--------------------------------------------------------------------------------
sizeToTris :: Size -> [Triangle (V2 Float)]
sizeToTris (Size (V2 w h)) = [Triangle a b c, Triangle a c d]
    where [a,b,c,d] = [V2 0 0, V2 w 0, V2 w h, V2 0 h]

class ToTriangles a where
    toTriangles :: a -> [Triangle (V2 Float)]

instance ToTriangles a => ToTriangles [a] where
    toTriangles = concatMap toTriangles

instance ToTriangles Size where
    toTriangles = sizeToTris

instance ToTriangles (Bezier (V2 Float)) where
    toTriangles (Bezier _ a b c) = [Triangle a b c]

instance Hashable a => Hashable (Triangle a) where
    hashWithSalt s (Triangle a b c) =
        s `hashWithSalt`  a `hashWithSalt` b `hashWithSalt` c
--------------------------------------------------------------------------------
-- Filling things
--------------------------------------------------------------------------------
data FillPrimitives = FillBeziers Fill [Bezier (V2 Float)]
                    | FillTriangles Fill [Triangle (V2 Float)]
                    | FillPaths Fill [Path]
                    | FillText Fill Font Float String

fillPrimsString :: FillPrimitives -> String
fillPrimsString (FillBeziers _ _) = "FillBeziers"
fillPrimsString (FillTriangles _ _) = "FillTriangles"
fillPrimsString (FillPaths _ _) = "FillPaths"
fillPrimsString (FillText _ _ _ _) = "FillText"

fillPrimsFill :: FillPrimitives -> Fill
fillPrimsFill (FillBeziers f _) = f
fillPrimsFill (FillTriangles f _) = f
fillPrimsFill (FillPaths f _) = f
fillPrimsFill (FillText f _ _ _) = f

fillPrimsPoints :: FillPrimitives -> [[V2 Float]]
fillPrimsPoints (FillBeziers _ bs) = [trisToComp $ toTriangles bs]
fillPrimsPoints (FillTriangles _ ts) = [trisToComp ts]
fillPrimsPoints (FillPaths _ ps) = map unPath ps
fillPrimsPoints _ = []

instance Show Fill where
    show (FillColor _) = "FillColor"
    show (FillTexture p _) = "FillTexture " ++ show p

deriving instance Generic FontStyle
instance Hashable FontStyle
deriving instance Generic FontDescriptor
instance Hashable FontDescriptor

instance Hashable FillPrimitives where
    hashWithSalt s (FillText f fd px str) =
            s `hashWithSalt` "FillText"
                `hashWithSalt` show f `hashWithSalt` show fd
                    `hashWithSalt` px `hashWithSalt` str
    hashWithSalt s fp
        | FillColor f <- fillPrimsFill fp =
            s `hashWithSalt` fillPrimsString fp
                `hashWithSalt` show (FillColor f)
                    `hashWithSalt` map (map f) (fillPrimsPoints fp)
        | FillTexture p f <- fillPrimsFill fp =
            s `hashWithSalt` fillPrimsString fp
                `hashWithSalt` show (FillTexture p f)
                    `hashWithSalt` p
                        `hashWithSalt` map (map f) (fillPrimsPoints fp)
        | otherwise = s

--instance Transformable Transform FillPrimitives where
--    transform t (FillBeziers f bs) = FillBeziers f $ transform t bs
--    transform t (FillTriangles f bs) = FillTriangles f $ transform t bs
--    transform t (FillPaths f bs) = FillPaths f $ transform t bs

path2ConcavePoly :: Path -> [Triangle (V2 Float)]
path2ConcavePoly (Path vs)
    | length vs >= 3
    , x:xs <- vs = zipWith (Triangle x) xs (drop 1 xs)
    | otherwise = []

--instance BoundedByBox FillPrimitives where
--    type BoundingBoxR FillPrimitives = FontMap
--    type BoundingBox FillPrimitives = BBox
--    boundingBox m (FillText _ fd w s) = boundingBox m $ PathText fd w s
--    boundingBox _ prims = boundingBox () $ concat $ fillPrimsPoints prims

instance Primitive FillPrimitives where
    type PrimM FillPrimitives = IO
    type PrimR FillPrimitives = Rez
    type PrimT FillPrimitives = Transform
    canAllocPrimitive _ _ = True
    compilePrimitive (Rez sh win _ _) (FillText fill font px str)
        | FillColor f <- fill = do
            let gsh = _shGeometry sh
                bsh = _shBezier sh
            Rendering r c <- colorFontRendering win gsh bsh
                                                (FontString font px (0,0) str)
                                                f
            return (c, r)
        -- TODO: FillText with texture fill
        | otherwise = return (return (), const $ return ())
    compilePrimitive (Rez sh win _ _) (FillBeziers fill bs) = do
        let bsh = _shBezier sh
        Rendering f c <- filledBezierRendering win bsh bs fill
        return (c, f)
    compilePrimitive (Rez sh win _ _) (FillTriangles fill ts) = do
        let gsh = _shGeometry sh
        Rendering f c <- filledTriangleRendering win gsh ts fill
        return (c, f)
    compilePrimitive (Rez sh win _ _) (FillPaths fill ps) = do
        -- We use a filled concave polygon technique instead of
        -- triangulating the path.
        -- http://www.glprogramming.com/red/chapter14.html#name13
        let gsh = _shGeometry sh
            tss = map path2ConcavePoly ps
        rs <- forM tss $ \ts -> do
            Rendering f c <- filledTriangleRendering win gsh ts fill
            return $ Rendering (\t -> stencilMask (f t) (f t)) c
        let Rendering f c = foldl (<>) mempty rs
        return (c,f)
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
stringRendering :: Window -> GeomShader -> BezShader -> Font
                -> String -> Color -> (Float,Float) -> IO G.Rendering
stringRendering win geom bz font str fc xy = do
    -- Some docs
    let mult = 2 :: Float
        movv = 1/4 :: Double
        movh = 1/2 :: Double
        px   = mult*16
        fstr = FontString font px xy str
        fc'  = fc `alpha` 0.25
        t    = Transform 0 (V2 (1/mult) (1/mult)) 0
    G.Rendering r1 c1 <- colorFontRendering win geom bz fstr $ const fc'
    G.Rendering r2 c2 <- colorFontRendering win geom bz fstr $ const fc
    let f t' = do r1 (translate (-movh) 0 t')
                  r1 (translate movh 0 t')
                  r1 (translate 0 movv t')
                  r1 (translate 0 (-movv) t')
                  r2 t'
    return $ transformRendering t $ G.Rendering f (c1 >> c2)
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
                           , wsRequests :: [Request]
                           -- ^ The mutable data for our control structures
                           }
