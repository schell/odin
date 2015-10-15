{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Odin.Control.Login where

import Odin.Data as O
import Odin.Control.Common
import Data.Text (pack)
--import Data.Renderable
import Caltrops.Client
--import Linear hiding (trace, el)
--import Gelatin.Core.Color
--import Gelatin.Core.Rendering
import Control.Varying
--import Odin.GUI
import Control.Monad.IO.Class

--loginCard :: Odin LoginCookie
--loginCard = loginAttempt Nothing
--
--loginAttempt :: Maybe (String, String, String) -> Odin LoginCookie
--loginAttempt mcreds = do
--    (e,p) <- case mcreds of
--        Just (e,p, m) -> errEmailAndPassword e p m
--        Nothing       -> getEmailAndPassword
--    res <- getLoginCookie e p
--    case res of
--        Right (Just ck) -> return ck
--        _   -> loginAttempt $ Just (e,p,"Please try again.")
--
--errorMsg :: String -> Var ControlM InputEvent (Event a) -> Odin ()
--errorMsg s ve = gui err ve >> return ()
--    where err :: Monad m => Var m i Component
--          err = pure (mempty, Element $ PlainText s red)
--
--getEmailAndPassword :: Odin (String, String)
--getEmailAndPassword = uncurry f <$> gui emptyLoginCard enter
--    where f l _ = (O.loginEmail l, O.loginPassword l)
--
--errEmailAndPassword :: String -> String -> String -> Odin (String, String)
--errEmailAndPassword a b e = combineGUI card err
--    where card = gui (loginCardWith a b) enter $ \l _ ->
--                     (O.loginEmail l, O.loginPassword l)
--          err  = transformGUI (pure t) $ errorMsg e enter
--          t    = Transform (V2 16 92) 1 0
--
--getLoginCookie :: String -> String
--               -> Odin (Either String (Maybe LoginCookie))
--getLoginCookie n p = gui spinner (fetchLoginCookie n p) $ \_ mck -> mck
--    where spinner = (,) <$> tfrm <*> icon
--          tfrm = Transform statusPos 1 <$> (time ~> r)
--          r = tween linear 0 (2*pi) 1 `andThen` r
--          icon = pure $ Element $ Icon "\xf1ce" white

fetchLoginCookie :: MonadIO m
                 => String -> String
                 -> Var m a (Event (Either String (Maybe LoginCookie)))
fetchLoginCookie n p = caltrops $ login localhost $ Login (pack n) (pack p)

--statusPos :: V2 Float
--statusPos = V2 130 40
--
--emptyLoginCard :: Monad m => Var m InputEvent LoginCard
--emptyLoginCard = loginCardWith "" ""
--
--loginCardWith :: Monad m => String -> String -> Var m InputEvent LoginCard
--loginCardWith s1 s2 = uncurry (LoginCard mempty)
--    <$> tabTuple s1 s2
--    <*> ((`mod` 2) <$> tabCount)

tabTuple :: Monad m => String -> String -> Var m InputEvent (String,String)
tabTuple s1 s2 = (,) <$> typingBufferOn s1 (t 0) <*> typingBufferOn s2 (t 1)
    where t n = tabCount ~> onWhen (\c -> c `mod` 2 == n)
