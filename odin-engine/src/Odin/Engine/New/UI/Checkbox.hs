{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Odin.Engine.New.UI.Checkbox
  ( checkbox
  ) where

import           Reflex.SDL2

import           Data.Char.FontAwesome
import           Odin.Engine.New
import           Odin.Engine.New.UI.Button


data CheckboxUpdate = CheckboxSetIsOn Bool
                    | CheckboxToggle


newtype CheckboxInternal = CB { cbIsToggled :: Bool }


foldCheckbox
  :: CheckboxInternal
  -> CheckboxUpdate
  -> CheckboxInternal
foldCheckbox cb up
  | CheckboxSetIsOn isOn <- up = CB isOn
  | CheckboxToggle       <- up = CB $ not $ cbIsToggled cb


checkbox
  :: OdinWidget r t m
  => Bool
  -> Event t Bool
  -> m (Dynamic t Bool, Dynamic t ButtonState)
checkbox iniIsOn evIsOn = mdo
  dSt <- iconButton faSquareO $ def & setData .~ evText
  let evUpdate = leftmost [ CheckboxSetIsOn <$> evIsOn
                          , CheckboxToggle  <$  buttonClickedEvent dSt
                          ]
  dCB <- accum foldCheckbox (CB iniIsOn) evUpdate
  let toChr b = if b then faCheckSquareO else faSquareO
  evText <- toChr <$$> delayEventOneFrame (cbIsToggled <$> updated dCB)
  (, dSt) <$> holdUniqDyn (cbIsToggled <$> dCB)
