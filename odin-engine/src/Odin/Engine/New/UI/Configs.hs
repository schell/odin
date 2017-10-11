{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}
module Odin.Engine.New.UI.Configs
  ( module Odin.Engine.New.UI.Configs
  , module L
  , def
  ) where


import           Control.Lens                as L ((&), (.~), (^.))
import           Control.Lens                (makeFields)
import           Data.Default                (Default, def)
import           Gelatin.GL
import           GHC.Generics                (Generic)
import           Reflex.SDL2                 (Event, Reflex, never)

import           Odin.Engine.New             (FontDescriptor, Shape)
import           Odin.Engine.New.UI.Painting (Painter)

instance Reflex t => Default (Event t a) where
  def = never

data TextFieldCfg t =
  TextFieldCfg { textFieldCfgSetTextEvent           :: Event t String
               , textFieldCfgSetColorEvent          :: Event t (V4 Float)
               , textFieldCfgSetFontDescriptorEvent :: Event t FontDescriptor
               , textFieldCfgSetTransformEvent      :: Event t [RenderTransform2]
               --, textFieldCfgFreeEvent              :: Event t ()
               } deriving (Generic, Default)
$(makeFields ''TextFieldCfg)


data PictureCfg vert t =
  PictureCfg { pictureCfgSetPictureEvent   :: Event t (Picture GLuint vert ())
             , pictureCfgSetTransformEvent :: Event t [RenderTransform2]
             } deriving (Generic, Default)
$(makeFields ''PictureCfg)


data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown
                 | ButtonStateClicked
                 deriving (Show, Eq, Ord, Enum, Bounded)


data ButtonData = ButtonData { buttonDataText  :: String
                             , buttonDataState :: ButtonState
                             }

data ButtonCfg t =
  ButtonCfg { buttonCfgSetTextEvent          :: Event t String
            , buttonCfgSetButtonPainterEvent :: Event t (Painter ButtonData IO)
            , buttonCfgSetTransformEvent     :: Event t [RenderTransform2]
            } deriving (Generic, Default)
$(makeFields ''ButtonCfg)


data TextInputState = TextInputStateUp
                    | TextInputStateOver
                    | TextInputStateDown
                    | TextInputStateEditing
                    | TextInputStateEdited
                    deriving (Show, Eq, Ord, Enum, Bounded)

data TextInputData = TextInputData { textInputText        :: String
                                   , textInputPlaceholder :: String
                                   , textInputState       :: TextInputState
                                   }

data TextInputCfg t =
  TextInputCfg { textInputCfgSetTextEvent             :: Event t String
               , textInputCfgSetPlaceholderTextEvent  :: Event t String
               , textInputCfgSetTextInputPainterEvent :: Event t (Painter TextInputData IO)
               , textInputCfgSetTransformEvent        :: Event t [RenderTransform2]
               } deriving (Generic, Default)
$(makeFields ''TextInputCfg)


data LayerCfg t =
  LayerCfg { layerCfgSetTransformEvent :: Event t [RenderTransform2]
           , layerCfgSetBoundaryEvent  :: Event t Shape
           } deriving (Generic, Default)
$(makeFields ''LayerCfg)
