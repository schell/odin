{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Odin.Data.Common where

import Odin.Graphics.Types
import Graphics.GL.Core33
import Graphics.UI.GLFW
import Graphics.Text.TrueType
import Gelatin.Core.Rendering as G
import Gelatin.Core.Color
import Linear hiding (mult)
import Data.Typeable
import Data.Hashable
import Data.IORef
import Data.Renderable
import GHC.Generics (Generic)
import Control.Lens.TH
import Control.GUI
import Control.Monad.Reader
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
data Input = Input { inputCursorPos :: V2 Float
                   , inputWindowSize :: V2 Float
                   } deriving (Show, Eq)

type InputM = ReaderT Input IO
type Odin = SplineT InputM [] InputEvent Component
type Varying = Var InputM InputEvent

type Component = (Transform, Element IO Rez Transform)
type UI = [Component]

instance Primitive () where
    type PrimM () = IO
    type PrimR  () = Rez
    type PrimT  () = Transform
    compilePrimitive _ _ = return (return (), const $ return ())
--------------------------------------------------------------------------------
-- Path
--------------------------------------------------------------------------------
type Path = [V2 Float]
--------------------------------------------------------------------------------
-- Polyline
--------------------------------------------------------------------------------
instance Primitive Polyline where
    type PrimM Polyline = IO
    type PrimR  Polyline = Rez
    type PrimT  Polyline = Transform
    compilePrimitive (Rez geom _ _ win _ _) Polyline{..} = do
        let fill = solid polylineColor
            p = polyline EndCapSquare LineJoinMiter polylineWidth polylinePath
        Rendering f c <- filledTriangleRendering win geom p fill
        return (c, f)

instance Hashable Polyline where
    hashWithSalt s Polyline{..} =
        s `hashWithSalt` polylineWidth
            `hashWithSalt` polylineColor
                `hashWithSalt` polylinePath

data Polyline = Polyline { polylineWidth     :: Float
                         , polylineColor     :: Color
                         , polylinePath      :: [V2 Float]
                         } deriving (Show, Eq, Typeable, Generic)

path2Polyline :: Float -> Color -> Path -> Polyline
path2Polyline = Polyline
--------------------------------------------------------------------------------
-- Box
--------------------------------------------------------------------------------
boxPath :: V2 Float -> Path
boxPath (V2 w h) = poly
    where poly = [V2 x1 y1, V2 x2 y1, V2 x2 y2, V2 x1 y2, V2 x1 y1]
          x1 = 0
          x2 = w
          y1 = 0
          y2 = h

boxPolyline :: Float -> Box -> Polyline
boxPolyline lw Box{..} = Polyline lw boxColor path
    where path = [V2 x1 y1, V2 x2 y1, V2 x2 y2, V2 x1 y2, V2 x1 y1]
          (V2 w h) = boxSize
          x1 = -hw
          x2 = w + hw
          y1 = -hw
          y2 = h + hw
          hw = lw/2

data Box = Box { boxSize      :: Size
               , boxColor     :: Color
               } deriving (Show, Eq, Typeable, Generic)
$(makeLensesFor [("boxSize", "boxSize_")
                ,("boxColor", "boxColor_")
                ] ''Box)

emptyBox :: Box
emptyBox = Box { boxSize = 0
               , boxColor = 0
               }

instance Hashable Box where
    hashWithSalt s (Box sz c) = s `hashWithSalt` sz `hashWithSalt` c

instance Primitive Box where
    type PrimM Box = IO
    type PrimR Box  = Rez
    type PrimT Box  = Transform
    compilePrimitive (Rez geom _ _ win _ _) (Box (V2 w h) c) = do
        let [tl, tr, br, bl] = [zero, V2 w 0, V2 w h, V2 0 h]
            vs = [tl, tr, br, tl, br, bl]
            cs = replicate 6 c
        Rendering f c' <- colorRendering win geom GL_TRIANGLES vs cs
        return (c',f)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
stringRendering :: Window -> GeomRenderSource -> BezRenderSource -> Font
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
                           , wsInput :: Input
                           -- ^ The input state
                           }
--------------------------------------------------------------------------------
-- Icon
--------------------------------------------------------------------------------
instance Primitive Icon where
    type PrimM Icon = IO
    type PrimR Icon = Rez
    type PrimT Icon = Transform
    compilePrimitive (Rez geom bz _ win _ font) (Icon str fc) = do
        Rendering f c <- stringRendering win geom bz font str fc iconOffset
        return (c,f)

iconOffset :: (Float, Float)
iconOffset = (-16,11.5)

iconReset :: Transform
iconReset = Transform (V2 16 (-11.5)) 1 0

instance Hashable Icon where
    hashWithSalt s (Icon t c) = s `hashWithSalt` t `hashWithSalt` c

data Icon = Icon { iconString :: String
                 , iconColor  :: Color
                 } deriving (Show, Eq)
$(makeLensesFor [("iconTransform", "iconTransform_")
                ,("iconString", "iconString_")
                ,("iconColor", "iconColor_")
                ] ''Icon)
--------------------------------------------------------------------------------
-- PlainText
--------------------------------------------------------------------------------
instance Primitive PlainText where
    type PrimM PlainText = IO
    type PrimR PlainText = Rez
    type PrimT PlainText = Transform
    compilePrimitive (Rez geom bz _ win font _) (PlainText str fc) = do
        Rendering f c <- stringRendering win geom bz font str fc (0,32)
        return (c,f)

instance Hashable PlainText where
    hashWithSalt s PlainText{..} =
        s `hashWithSalt` plainTxtString `hashWithSalt` plainTxtColor

data PlainText = PlainText { plainTxtString :: String
                           , plainTxtColor  :: Color
                           } deriving (Show, Eq, Generic)
$(makeLensesFor [("plainTxtString", "plainTxtString_")
                ,("plainTxtColor", "plainTxtColor_")
                ] ''PlainText)

emptyPlainText :: PlainText
emptyPlainText = PlainText { plainTxtString = ""
                           , plainTxtColor = 0
                           }
--------------------------------------------------------------------------------
-- TextInput
--------------------------------------------------------------------------------
data TextInput = TextInput { textInputTransform :: Transform
                           , textInputText      :: PlainText
                           , textInputBox       :: Box
                           , textInputPos       :: Int
                           , textInputActive    :: Bool
                           } deriving (Show, Eq, Typeable)
$(makeLensesFor [("textInputTransform", "textInputTransform_")
                ,("textInputText", "textInputText_")
                ,("textInputBox", "textInputBox_")
                ,("textInputPos", "textInputPos_")
                ,("textInputActive", "textInputActive_")
                ] ''TextInput)

localTextInputPath :: TextInput -> Path
localTextInputPath = boxPath . boxSize . textInputBox

globalTextInputPath :: TextInput -> Path
globalTextInputPath t@TextInput{..} =
    transformPoly textInputTransform $ localTextInputPath t

textInputOutline :: TextInput -> Polyline
textInputOutline t@TextInput{..} = path2Polyline 1 white $ localTextInputPath t

emptyTextInput :: TextInput
emptyTextInput = TextInput { textInputTransform = mempty
                           , textInputText = emptyPlainText
                           , textInputBox = emptyBox
                           , textInputPos = 0
                           , textInputActive = False
                           }

instance Composite TextInput [] IO Rez Transform where
    composite txt@TextInput{..} =
        [ (textInputTransform, Element textInputBox)
        , (textInputTransform, Element textInputText)
        ] ++ [(textInputTransform, Element poly) | textInputActive]
            where poly = textInputOutline txt
