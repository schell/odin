module Odin.Control.TextInput (
    textInput,
    textInputPath,
    defaultTextInput,
    inactiveTextInput,
    activeTextInput,
    clickInTextInput,
    clickOutTextInput,
) where

import Odin.Data.Common
import Odin.Control.Common
import Odin.GUI
import Control.Monad
import Control.Varying
import Control.Applicative
import Control.Monad.Trans.RWS.Strict
import Control.Monad.IO.Class
import Control.Lens hiding ((<~))
import Linear
import Gelatin.Core.Color
import Gelatin.Core.Rendering

defaultTextInput :: TextInput
defaultTextInput = mutate emptyTextInput $ do
    textInputBox._2.boxColor .= V4 0.3 0.3 0.3 1
    textInputBox._2.boxSize .= V2 200 20

textInput :: Varying Transform -> TabIndex -> Maybe TextInput -> Odin TextInput
textInput vt ti Nothing = textInput vt ti $ Just defaultTextInput
textInput vt (TabIndex ndx) (Just t) = do
    liftIO $ putStrLn "inactive"
    let inactive = inactiveTextInput vt t
        activate = msum <$> sequenceA [clickInTextInput inactive
                                      ,tabCount ~> onWhen (== ndx) ~> var (() <$)
                                      ]
    t' <- fst <$> gui inactive activate

    let active = activeTextInput vt t'
        deactivate = msum <$> sequenceA [clickOutTextInput active
                                        ,dropE 1 tab
                                        ]
    liftIO $ putStrLn "active"
    (ui, (txt,_)) <- capture $ gui active deactivate
    wait 1 ui
    return txt

clickOutTextInput, clickInTextInput :: Varying TextInput
                                    -> Varying (Event ())
clickInTextInput = leftClickInPath . (textInputPath <$>)
clickOutTextInput = leftClickOutPath . (textInputPath <$>)

-- | An inactive text input reacts to the mouse by highlighting its
-- bounding box. The gui ends once the mouse clicks inside the field's bounding
-- box.
inactiveTextInput :: Varying Transform -> TextInput -> Varying TextInput
inactiveTextInput vt t = textinput
    where textinput = TextInput <$> text
                                <*> box
                                <*> 0
                                <*> pure False
          box = (,) <$> tfrm <*> (Box <$> bxsz <*> bxclr)
          text = (,) <$> tfrm <*> pure (t^.textInputText._2)
          bxclr = (t^.textInputBox._2.boxColor ^*) <$> colorMult path
          bxsz = pure $ t^.textInputBox._2.boxSize
          tfrm = vt
          path = transformPoly <$> tfrm <*> (boxPath <$> bxsz)

-- | An active text input collects typed text in a buffer. It ends once the
-- user clicks ouside of the field's bounding box.
activeTextInput :: Varying Transform -> TextInput -> Varying TextInput
activeTextInput vt t = textinput
    where textinput = TextInput <$> text
                                <*> box
                                <*> 0
                                <*> pure True
          box = (,) <$> vt <*> (Box <$> (str ~> varM (\s -> textInputBoxSize s 200 500))
                                    <*> pure (t^.textInputBox^._2.boxColor))
          text = (,) <$> vt <*> (PlainText <$> str <*> strclr)
          str = typingBufferOn (t^.textInputText._2.plainTextString) (always ())
          strclr = pure white

textInputBoxSize :: (Monad m, Monoid w)
                  => String -> Float -> Float
                  -> (RWST ReadData w s m) (V2 Float)
textInputBoxSize s mn mx = do
    sz <- textSize s
    let V2 w h = sz + V2 2 0
        h' = if h < 20 then 20 else h
        w' = max mn (min w mx)
    return $ V2 w' h'

colorMult :: (Monad m, Monoid w)
          => Var (RWST ReadData w s m) InputEvent Path
          -> Var (RWST ReadData w s m) InputEvent Float
colorMult vpath = 0.8 `orE` ((1 <$) <$> cursorInPath vpath)
