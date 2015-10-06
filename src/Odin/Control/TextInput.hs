{-# LANGUAGE RecordWildCards #-}
module Odin.Control.TextInput where

import Odin.Data.Common
import Odin.Control.Common
import Control.Varying
import Control.GUI
import Control.Monad.IO.Class
import Linear
import Gelatin.Core.Color
import Gelatin.Core.Rendering

testTextInput :: Odin TextInput
testTextInput = do
    txt <- emptyTextInput
    liftIO $ putStrLn "got click out"
    txt' <- enteringTextInput txt
    liftIO $ putStrLn "blaaaah!"
    return txt'

emptyTextInput :: Odin TextInput
emptyTextInput = gui textinput (leftClickInPath path) const
    where textinput = TextInput <$> tfrm
                                <*> text
                                <*> box
                                <*> 0
                                <*> pure False
          box = Box <$> bxsz
                    <*> bxclr
          text = PlainText <$> str
                           <*> strclr
          str = pure ""
          strclr = pure white
          bxclr = (V4 0.3 0.3 0.3 1 ^*) <$> colorMult path
          bxsz = pure $ V2 200 18
          tfrm = Transform <$> pos <*> 1 <*> 0
          pos  = pure 100
          path = transformPoly <$> tfrm <*> (boxPath <$> bxsz)

enteringTextInput :: TextInput -> Odin TextInput
enteringTextInput TextInput{..} = gui textinput (dropE 1 $ leftClickOutPath path) const
    where textinput = TextInput <$> tfrm
                                <*> text
                                <*> box
                                <*> 0
                                <*> pure False
          box = pure textInputBox
          text = PlainText <$> str
                           <*> strclr
          str = pure ""
          strclr = pure white
          bxclr = (V4 0.3 0.3 0.3 1 ^*) <$> colorMult path
          bxsz = pure $ V2 200 18
          tfrm = pure textInputTransform
          path = transformPoly <$> tfrm <*> (boxPath <$> bxsz)

textInputPath :: Monad m => Var m i TextInput -> Var m i Path
textInputPath vtxt = globalTextInputPath <$> vtxt

colorMult :: Monad m => Var m InputEvent Path -> Var m InputEvent Float
colorMult vpath = 0.8 `orE` ((1 <$) <$> cursorInPath vpath)

clickableFieldString :: Monad m
                => Var m InputEvent Path -> Var m InputEvent String
clickableFieldString _ = undefined
