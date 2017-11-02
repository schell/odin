{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
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
               } deriving (Generic, Default)
$(makeFields ''TextFieldCfg)


data PictureCfg vert t =
  PictureCfg { pictureCfgSetPictureEvent   :: Event t (Picture GLuint vert ())
             } deriving (Generic, Default)
$(makeFields ''PictureCfg)


data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown
                 | ButtonStateClicked
                 deriving (Show, Eq, Ord, Enum, Bounded)


data ButtonData a = ButtonData { buttonData      :: a
                               , buttonDataState :: ButtonState
                               }

buttonDataText :: ButtonData String -> String
buttonDataText = buttonData

data ButtonCfg t a =
  ButtonCfg { buttonCfgSetData          :: Event t a
            , buttonCfgSetButtonPainter :: Event t (Painter (ButtonData a) IO)
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
               } deriving (Generic, Default)
$(makeFields ''TextInputCfg)


data LayerCfg t =
  LayerCfg { layerCfgSetBoundaryEvent  :: Event t Shape
           } deriving (Generic, Default)
$(makeFields ''LayerCfg)


data PaneCfg t =
  PaneCfg { paneCfgSetBoundaryEvent  :: Event t Shape
          , paneCfgSetOffsetEvent    :: Event t (V2 Float)
          } deriving (Generic, Default)
$(makeFields ''PaneCfg)
