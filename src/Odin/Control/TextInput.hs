{-# LANGUAGE RecordWildCards #-}
module Odin.Control.TextInput (
    testTextInput,
    inactiveTextInput,
    activeTextInput,
    textInputPath
) where

import Odin.Data.Common
import Odin.Control.Common
import Control.Varying
import Control.Varying.Spline
import Control.GUI
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Lens hiding ((<~))
import Linear
import Gelatin.Core.Color
import Gelatin.Core.Rendering

testTextInput :: TextInput -> Odin TextInput
testTextInput t@TextInput{..} = do
    let path = textInputPath t

    t' <- gui (inactiveTextInput t) (leftClickInPath $ pure path) const

    t'' <- gui (activeTextInput t') (leftClickOutPath $ pure path) const

    liftIO $ putStrLn "done editing text input"
    let str = t''^.textInputText_.plainTxtString_

    liftIO $ putStrLn str
    testTextInput t''

textInputPath :: TextInput -> Path
textInputPath TextInput{..} = path
    where path = transformPoly textInputTransform (boxPath bxsz)
          bxsz = textInputBox^.boxSize_

test :: MonadIO m => Spline m Float Float Float
test = do
    x <- tweenTo easeOutExpo 0 100 1
    liftIO $ putStrLn "halfway there"
    tweenTo easeOutExpo x 0 1

textInput :: MonadIO m
          => TextInput -> Spline (ReaderT Input m) InputEvent TextInput ()
textInput t = do
    let path = pure $ textInputPath t
        clickIn = leftClickInPath path
        clickOut = leftClickOutPath path

    t' <- varyUntilEvent (inactiveTextInput t) clickIn const

    liftIO $ putStrLn "now editing text input"
    t'' <- varyUntilEvent (activeTextInput t') clickOut const

    let str = t''^.textInputText_.plainTxtString_
    liftIO $ putStrLn $ "got: " ++ str

    textInput t''

-- | An inactive text input reacts to the mouse by highlighting its
-- bounding box. The gui ends once the mouse clicks inside the field's bounding
-- box.
inactiveTextInput :: Monad m
                  => TextInput -> Var (ReaderT Input m) InputEvent TextInput
inactiveTextInput TextInput{..} = textinput
    where textinput = TextInput <$> tfrm
                                <*> text
                                <*> box
                                <*> 0
                                <*> pure False
          box = Box <$> bxsz
                    <*> bxclr
          text = pure textInputText
          bxclr = (textInputBox^.boxColor_ ^*) <$> colorMult path
          bxsz = pure $ textInputBox^.boxSize_
          tfrm = pure textInputTransform
          path = transformPoly <$> tfrm <*> (boxPath <$> bxsz)

-- | An active text input collects typed text in a buffer. It ends once the
-- user clicks ouside of the field's bounding box.
activeTextInput :: Monad m
                => TextInput -> Var (ReaderT Input m) InputEvent TextInput
activeTextInput TextInput{..} = textinput
    where textinput = TextInput <$> tfrm
                                <*> text
                                <*> box
                                <*> 0
                                <*> pure True
          box = pure textInputBox
          text = PlainText <$> str
                           <*> strclr
          str = typingBufferOn (plainTxtString textInputText) (always ())
          strclr = pure white
          tfrm = pure textInputTransform
          bxsz = pure $ textInputBox^.boxSize_
          path = transformPoly <$> tfrm <*> (boxPath <$> bxsz)

colorMult :: Monad m
          => Var (ReaderT Input m) InputEvent Path
          -> Var (ReaderT Input m) InputEvent Float
colorMult vpath = 0.8 `orE` ((1 <$) <$> cursorInPath vpath)
