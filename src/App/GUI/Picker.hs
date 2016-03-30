module App.GUI.Picker where

import           Gelatin.Picture
import           Control.Monad (forM_)
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)
import           Linear

data Picker a = Picker { pickerMap :: IntMap a
                       , pickerHoverChoice :: Int
                       , pickerHoverChoiceDown :: Bool
                       }

data PickerPainter a t = PickerPainter
  { paintPickerItem :: a -> Bool -> Bool -> Picture t ()
  , positionPickerItem :: Int -> V2 Float
  }

paintPicker :: PickerPainter a t -> Picker a -> Picture t ()
paintPicker pnt p = do
  forM_ (IM.toList $ pickerMap p) $ \(i, item) ->
    move (positionPickerItem pnt i) $
      paintPickerItem pnt item (i == pickerHoverChoice p)
                               (pickerHoverChoiceDown p)
