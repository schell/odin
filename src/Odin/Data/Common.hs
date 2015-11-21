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
import Graphics.Text.TrueType
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

-- | Input is a type that stores certain values pertaining to user input.
-- Without storing things like the last cursor position our varying values
-- have to wait for an event before taking a real value. This way all
-- varying values that rely on cursor position can have a (usable) value
-- instantly.
data ReadData = ReadData { _readCursorPos :: V2 Float
                         , _readWindowSize :: V2 Float
                         , _readResources :: Rez
                         , _readDpi :: Dpi
                         }
makeLenses ''ReadData

data StateData = StateData { _statePrintUISteps :: Bool }
makeLenses ''StateData

type ControlM = RWST ReadData () StateData IO
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
-- Path
--------------------------------------------------------------------------------
newtype Path = Path { unPath :: [V2 Float] } deriving (Show, Generic)

instance Transformable Transform Path where
    transform t (Path vs) = Path $ transform t vs

instance Hashable Path

data PathPrimitives = Paths [Path]
                    | PathText FontDescriptor Float String
                    deriving (Generic, Show)

instance Hashable PathPrimitives
--------------------------------------------------------------------------------
-- Polyline
--------------------------------------------------------------------------------
--instance Primitive Polyline where
--    type PrimM Polyline = IO
--    type PrimR  Polyline = Rez
--    type PrimT  Polyline = Transform
--    compilePrimitive (Rez sh win _ _) Polyline{..} = do
--        let cs = map snd _polylinePath
--            vs = map fst _polylinePath
--            shader = _shProjectedPolyline sh
--        Rendering f c <- projectedPolylineRendering win shader _polylineWidth
--                             _polylineFeather _polylineCaps vs cs
--        return (c, f)
--
--instance Hashable LineCap where
--    hashWithSalt s c = s `hashWithSalt` show c
--
--instance Hashable Polyline where
--    hashWithSalt s Polyline{..} =
--        s `hashWithSalt` _polylineWidth
--            `hashWithSalt` _polylineFeather
--                `hashWithSalt` _polylinePath
--                    `hashWithSalt` _polylineCaps
--
--data Polyline = Polyline { _polylineWidth     :: Float
--                         , _polylineFeather   :: Float
--                         , _polylinePath      :: [(V2 Float,V4 Float)]
--                         , _polylineCaps      :: (LineCap,LineCap)
--                         } deriving (Show, Eq, Typeable, Generic)
--makeLenses ''Polyline
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

--boxPolyline :: Float -> Box -> Polyline
--boxPolyline lw Box{..} = Polyline lw 0.5 (zip path $ repeat _boxColor) (LineCapSquare,LineCapSquare)
--    where path = [V2 x1 y1, V2 x2 y1, V2 x2 y2, V2 x1 y2, V2 x1 y1]
--          (V2 w h) = _boxSize
--          x1 = -hw
--          x2 = w + hw
--          y1 = -hw
--          y2 = h + hw
--          hw = lw/2
--
--data Box = Box { _boxSize      :: V2 Float
--               , _boxColor     :: Color
--               } deriving (Show, Eq, Typeable, Generic)
--makeLenses ''Box
--
--emptyBox :: Box
--emptyBox = Box { _boxSize = 0
--               , _boxColor = 0
--               }
--
--instance Hashable Box where
--    hashWithSalt s (Box sz c) = s `hashWithSalt` sz `hashWithSalt` c
--
--instance Primitive Box where
--    type PrimM Box = IO
--    type PrimR Box  = Rez
--    type PrimT Box  = Transform
--    compilePrimitive (Rez sh win _ _) (Box (V2 w h) c) = do
--        let geom = _shGeometry sh
--            [tl, tr, br, bl] = [zero, V2 w 0, V2 w h, V2 0 h]
--            vs = [tl, tr, br, tl, br, bl]
--            cs = replicate 6 c
--        Rendering f c' <- colorRendering win geom GL_TRIANGLES vs cs
--        return (c',f)
--------------------------------------------------------------------------------
-- Picture Helpers
--------------------------------------------------------------------------------
newtype Size = Size (V2 Float)
--------------------------------------------------------------------------------
-- Decomposing things into paths
--------------------------------------------------------------------------------
class ToPaths a where
    toPaths :: FontMap -> a -> [Path]

instance ToPaths [V2 Float] where
    toPaths _ vs = [Path vs]

instance ToPaths Size where
    toPaths _ (Size sz) = [boxPath sz]

instance ToPaths Path where
    toPaths _ p = [p]

instance ToPaths PathPrimitives where
    toPaths _ (Paths ps) = ps
    toPaths m (PathText fd px str) =
        case M.lookup fd m of
            Nothing -> []
            Just f  -> let qs = fontCurves 72 f px str
                           sub = subdivideAdaptive 100 0
                           mkPath = Path . cleanSeqDupes . concat . fmap sub
                       in concat $ fmap (fmap mkPath) qs

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

instance (ToPaths a, Show a) => Primitive (Stroked a) where
    type PrimM (Stroked a) = IO
    type PrimR (Stroked a) = Rez
    type PrimT (Stroked a) = Transform
    canAllocPrimitive (Rez _ _ _ m) (Stroked _ p) = not $ null $ toPaths m p
    compilePrimitive (Rez sh win _ m) (Stroked (Stroke c w f cp) p) = do
        let ps = toPaths m p
            cs = repeat c
            shader = _shProjectedPolyline sh
        rs <- forM ps $ \(Path vs) ->
            projectedPolylineRendering win shader w f cp vs cs
        let Rendering a b = foldl (<>) mempty rs
        return (b, a)
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
                    | FillText Fill FontDescriptor Float String

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
                `hashWithSalt` show f `hashWithSalt` fd
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

instance Primitive FillPrimitives where
    type PrimM FillPrimitives = IO
    type PrimR FillPrimitives = Rez
    type PrimT FillPrimitives = Transform
    canAllocPrimitive (Rez _ _ _ m) (FillText _ fd _ _) =
        case M.lookup fd m of
            Just _  -> True
            Nothing -> False
    canAllocPrimitive _ _ = True
    compilePrimitive (Rez sh win _ fm) (FillText fill fd px str)
        | FillColor f <- fill
        , Just font <- M.lookup fd fm = do
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
                           , wsState :: StateData
                           -- ^ The mutable data for our control structures
                           }
--------------------------------------------------------------------------------
-- Icon
--------------------------------------------------------------------------------
--instance Primitive Icon where
--    type PrimM Icon = IO
--    type PrimR Icon = Rez
--    type PrimT Icon = Transform
--    compilePrimitive (Rez sh win _ font) (Icon str fc) = do
--        let geom = _shGeometry sh
--            bz = _shBezier sh
--        Rendering f c <- stringRendering win geom bz font str fc iconOffset
--        return (c,f)
--
--iconOffset :: (Float, Float)
--iconOffset = (-16,11.5)
--
--iconReset :: Transform
--iconReset = Transform (V2 16 (-11.5)) 1 0
--
--instance Hashable Icon where
--    hashWithSalt s (Icon t c) = s `hashWithSalt` t `hashWithSalt` c
--
--data Icon = Icon { _iconString :: String
--                 , _iconColor  :: Color
--                 } deriving (Show, Eq)
--makeLenses ''Icon
--------------------------------------------------------------------------------
-- PlainText
--------------------------------------------------------------------------------
--instance Primitive PlainText where
--    type PrimM PlainText = IO
--    type PrimR PlainText = Rez
--    type PrimT PlainText = Transform
--    compilePrimitive (Rez sh win font _) (PlainText str fc) = do
--        let geom = _shGeometry sh
--            bz = _shBezier sh
--        Rendering f c <- stringRendering win geom bz font str fc (0,32)
--        return (c,f)
--
--instance Hashable PlainText where
--    hashWithSalt s PlainText{..} =
--        s `hashWithSalt` _plainTextString `hashWithSalt` _plainTextColor
--
--data PlainText = PlainText { _plainTextString :: String
--                           , _plainTextColor  :: Color
--                           } deriving (Show, Eq, Generic)
--makeLenses ''PlainText
--
--emptyPlainText :: PlainText
--emptyPlainText = PlainText { _plainTextString = ""
--                           , _plainTextColor = 0
--                           }
--------------------------------------------------------------------------------
-- TextInput
--------------------------------------------------------------------------------
--newtype TabIndex = TabIndex { _tabIndex :: Int }
--makeLenses ''TabIndex
--
--data TextInput = TextInput { _textInputText      :: (Transform, PlainText)
--                           , _textInputBox       :: (Transform, Box)
--                           , _textInputPos       :: Int
--                           , _textInputActive    :: Bool
--                           } deriving (Show, Eq, Typeable)
--makeLenses ''TextInput

--textInputPath :: TextInput -> Path
--textInputPath TextInput{..} = transformPoly t $ boxPath $ _boxSize box
--    where (t,box) = _textInputBox
--
--textInputOutline :: TextInput -> Polyline
--textInputOutline t@TextInput{..} = Polyline 1 0.5 (zip (textInputPath t) (repeat white)) (LineCapSquare, LineCapSquare)

--emptyTextInput :: TextInput
--emptyTextInput = TextInput { _textInputText = (mempty, emptyPlainText)
--                           , _textInputBox = (mempty, emptyBox)
--                           , _textInputPos = 0
--                           , _textInputActive = False
--                           }

--instance Composite TextInput [] IO Rez Transform where
--    composite txt@TextInput{..} =
--        [ Element <$> _textInputBox
--        , Element <$> _textInputText
--        ] ++ [(mempty, Element poly) | _textInputActive]
--            where poly = textInputOutline txt

--------------------------------------------------------------------------------
-- TextField
--------------------------------------------------------------------------------
--data TextField = TextField { _textFieldLabel :: (Transform, PlainText)
--                           , _textFieldInput :: TextInput
--                           , _textFieldError :: (Transform, PlainText)
--                           } deriving (Show, Eq)
--makeLenses ''TextField
--
--emptyTextField :: TextField
--emptyTextField = TextField { _textFieldLabel = (mempty, emptyPlainText)
--                           , _textFieldInput = emptyTextInput
--                           , _textFieldError = (mempty, emptyPlainText)
--                           }

--instance Composite TextField [] IO Rez Transform where
--    composite TextField{..} =
--       (fst _textFieldLabel, Element $ snd _textFieldLabel)
--       : composite _textFieldInput
--       ++
--       [ (fst _textFieldError, Element $ snd _textFieldError) ]
--------------------------------------------------------------------------------
-- TextForm
--------------------------------------------------------------------------------
--data TextForm = TextForm { _textFormFields   :: [TextField]
--                         , _textFormButton   :: TextInput
--                         , _textFormTabIndex :: Int
--                         }
--makeLenses ''TextForm
