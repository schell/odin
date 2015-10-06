{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Odin.Data.Login where

import Odin.Data.Common
import Odin.Graphics.Types
import Caltrops.Client
import Gelatin.Core.Rendering as G
import Gelatin.Core.Color
import Linear hiding (mult)
import Data.Typeable
import Data.Renderable as R
import Data.Monoid
import qualified Data.Text as T
import Control.Lens.TH
--------------------------------------------------------------------------------
-- Saving a login
--------------------------------------------------------------------------------
--data SavedLogin = SavedLogin { savedLoginButton :: Button
--                             , savedLoginCookie :: LoginCookie
--                             } deriving (Show, Eq)
--------------------------------------------------------------------------------
-- Logging in
--------------------------------------------------------------------------------
cardToLogin :: LoginCard -> Login
cardToLogin LoginCard{..} = Login (T.pack loginEmail) (T.pack loginPassword)

instance Composite LoginCard IO Rez Transform where
    composite LoginCard{..} = els
        where els = [ (loginTransform, Element $ Box (V2 400 100) $ V4 0.1 0.1 0.1 1)
                    , (loginTransform <> titleT, Element $ PlainText "Please Login" $
                        white `alpha` 0.5)
                    , (loginTransform <> emailT, Element $ PlainText "   Email:" emailClr)
                    ] ++ composite (TextInput (emailT <> h) emailTxt nullBox 0 False) ++
                    [ (loginTransform <> passT, Element $ PlainText "Password:" passClr)
                    ] ++ composite (TextInput (passT <> h) passTxt nullBox 0 False)
              t = Transform (V2 0 20) 1 0
              h = Transform (V2 100 0) 1 0
              titleT = Transform (V2 16 32) 1 0
              emailT = titleT <> t
              emailClr = if loginTabIndex == 0 then white
                            else white `alpha` 0.5
              emailTxt = PlainText loginEmail emailClr
              passT = emailT <> t
              passTxt = PlainText loginPassword passClr
              passClr = if loginTabIndex == 1 then white
                           else white `alpha` 0.5
              nullBox = Box 0 black

data LoginCard = LoginCard { loginTransform:: Transform
                           , loginEmail    :: String
                           , loginPassword :: String
                           , loginTabIndex :: Int
                           --, loginSaves    :: [SavedLogin]
                           } deriving (Show, Eq, Typeable)
$(makeLensesFor [("loginTransform", "loginTransform_")
                ,("loginEmail", "loginEmail_")
                ,("loginPassword", "loginPassword_")
                ,("loginTabIndex", "loginTabIndex_")
                --,("loginSaves", "loginSaves_")
                ] ''LoginCard)


