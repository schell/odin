{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
module Odin.Engine.New.UI.TextInput
  ( textInputWith
  , textInput
  , TextInputOutput(..)
  , textInputEditedEvent
  ) where

import           Control.Lens                hiding (to)
import           Control.Monad               (guard)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           Data.Word                   (Word64)
import           Gelatin.GL
import           Reflex.SDL2                 hiding (fan)
import           SDL.Raw.Enum                (pattern SDL_SYSTEM_CURSOR_HAND,
                                              SystemCursor)

import           Odin.Engine.New
import           Odin.Engine.New.UI.Configs
import           Odin.Engine.New.UI.Painters
import           Odin.Engine.New.UI.Painting


-- | The result of running a text input widget. Contains the source of truth
-- of the input's current state and text value.
data TextInputOutput t = TextInputOutput { tioState :: Dynamic t TextInputState
                                         , tioText  :: Dynamic t String
                                         }


-- | Retrieves an 'Event t Text' from a 'TextInputOutput t' that fires when the
-- input text is committed.
textInputEditedEvent
  :: Reflex t => TextInputOutput t -> Event t String
textInputEditedEvent tio =
  tagPromptlyDyn (tioText tio) $
    fmapMaybe (guard . (== TextInputStateEdited)) $ updated $ tioState tio



data TextInputUpdate = TextInputSetText String
                     | TextInputSetPlaceholderText String
                     | TextInputSetPainter (Painter TextInputData IO)
                     | TextInputMouseMotion MouseMotionEventData
                     | TextInputMouseButton  MouseButtonEventData
                     | TextInputTextInput TextInputEventData
                     | TextInputKeyboard KeyboardEventData


data TextInputInternal = TII { tiiK           :: Word64
                             , tiiState       :: TextInputState
                             , tiiText        :: String
                             , tiiLastText    :: String
                             , tiiPlaceholder :: String
                             , tiiMousePos    :: V2 Float
                             , tiiPainter     :: Painter TextInputData IO
                             , tiiPainting    :: TextInputState -> Painting
                             }


mkPaintings
  :: Painter TextInputData IO
  -> String
  -> String
  -> IO (TextInputState -> Painting)
mkPaintings painter txt placeholder = do
  let runInputPainter = runPainter painter . TextInputData txt placeholder
      states          = [minBound .. maxBound]
  paintings <- mapM runInputPainter states
  return $ \st -> fromMaybe mempty $ lookup st $ zip states paintings


tiiBoundary :: TextInputInternal -> Shape
tiiBoundary tii = paintingShape $ tiiPainting tii $ tiiState tii


tiiContainsMouse :: TextInputInternal -> Bool
tiiContainsMouse tii = shapeHasPoint (tiiBoundary tii) (tiiMousePos tii)


tiiRender :: TextInputInternal -> [RenderTransform2] -> IO ()
tiiRender tii = renderPainting $ tiiPainting tii $ tiiState tii


tiiFree :: TextInputInternal -> IO ()
tiiFree tii = mapM_ (freePainting . tiiPainting tii) [minBound .. maxBound]


tiiCursor :: TextInputInternal -> Maybe SystemCursor
tiiCursor tii = case tiiState tii of
  TextInputStateOver -> Just SDL_SYSTEM_CURSOR_HAND
  _                  -> Nothing



toWidget :: TextInputInternal -> Widget
toWidget tii = Widget (tiiK tii)
                      []
                      [tiiBoundary tii]
                      (tiiFree tii, tiiRender tii)
                      (tiiCursor tii)


foldTextInput
  :: MonadIO m
  => TVar Word64
  -> TextInputInternal
  -> TextInputUpdate
  -> m TextInputInternal
foldTextInput tvFresh st up
  | TextInputSetText txt     <- up = liftIO $ do
      k      <- freshWith tvFresh
      paints <- mkPaintings (tiiPainter st) txt (tiiPlaceholder st)
      return st{ tiiK = k, tiiPainting = paints , tiiText = txt, tiiLastText = txt }

  | TextInputSetPlaceholderText txt <- up = liftIO $ do
      k      <- freshWith tvFresh
      paints <- mkPaintings (tiiPainter st) (tiiText st) txt
      return st{ tiiK = k, tiiPainting = paints , tiiPlaceholder = txt }

  | TextInputSetPainter pntr <- up = liftIO $ do
      k      <- freshWith tvFresh
      paints <- mkPaintings pntr (tiiText st) (tiiPlaceholder st)
      return $ st{ tiiK = k, tiiPainter = pntr, tiiPainting = paints }

  | TextInputMouseMotion dat <- up
  , P v2Int  <- mouseMotionEventPos dat
  , st1      <- st{ tiiMousePos = fromIntegral <$> v2Int }
  , hasMouse <- tiiContainsMouse st1 = return $ case (tiiState st1, hasMouse) of
      (TextInputStateUp,      True) -> st1{ tiiState = TextInputStateOver }
      (TextInputStateOver,   False) -> st1{ tiiState = TextInputStateUp   }
      (TextInputStateEdited,  True) -> st1{ tiiState = TextInputStateOver }
      (TextInputStateEdited, False) -> st1{ tiiState = TextInputStateUp   }
      _                             -> st1

  | TextInputMouseButton dat <- up
  , ButtonLeft <- mouseButtonEventButton dat
  , P v2Int    <- mouseButtonEventPos dat
  , st1        <- st{ tiiMousePos = fromIntegral <$> v2Int }
  , hasMouse   <- tiiContainsMouse st1
  , motion     <- mouseButtonEventMotion dat = case (tiiState st, motion) of
      (TextInputStateOver,    Pressed) ->
        return st1{ tiiState = TextInputStateDown    }
      (TextInputStateDown,   Released) ->
        return $ if hasMouse
                 then st1{ tiiState = TextInputStateEditing }
                 else st1{ tiiState = TextInputStateUp      }
      (TextInputStateEditing, Pressed)
        | hasMouse                     -> return st1
        | tiiText st /= tiiLastText st -> return st1{ tiiState    = TextInputStateEdited
                                                    , tiiLastText = tiiText st
                                                    }
        | otherwise -> return st1{ tiiState = TextInputStateUp }
      _ -> return st1

  | TextInputTextInput dat <- up
  , txt                    <- textInputEventText dat
  , TextInputStateEditing  <- tiiState st = liftIO $ do
      let txt1 = tiiText st ++ T.unpack txt
      k      <- freshWith tvFresh
      paints <- mkPaintings (tiiPainter st) txt1 (tiiPlaceholder st)
      return st{ tiiK = k, tiiPainting = paints, tiiText = txt1 }

  | TextInputKeyboard dat <- up
  , Pressed               <- keyboardEventKeyMotion dat
  , TextInputStateEditing <- tiiState st
  , code                  <- keysymKeycode $ keyboardEventKeysym dat
  , not $ null $ tiiText st
  , code `elem` [KeycodeBackspace, KeycodeDelete] = liftIO $ do
      let txt1 = init $ tiiText st
      k      <- freshWith tvFresh
      paints <- mkPaintings (tiiPainter st) txt1 (tiiPlaceholder st)
      return st{ tiiK = k, tiiPainting = paints, tiiText = txt1 }

  | TextInputKeyboard dat <- up
  , Pressed               <- keyboardEventKeyMotion dat
  , TextInputStateEditing <- tiiState st
  , code                  <- keysymKeycode $ keyboardEventKeysym dat
  , code `elem` [KeycodeReturn, KeycodeReturn2] =
      return st{ tiiState = TextInputStateEdited, tiiLastText = tiiText st }

  | TextInputKeyboard dat <- up
  , Pressed               <- keyboardEventKeyMotion dat
  , TextInputStateEditing <- tiiState st
  , code                  <- keysymKeycode $ keyboardEventKeysym dat
  , code == KeycodeEscape =
    foldTextInput tvFresh st{ tiiState = TextInputStateUp } $ TextInputSetText (tiiLastText st)

  | otherwise = return st


-- | Creates a text input for the user to input text. Returns an output type
-- containing a 'Dynamic t TextInputState' and 'Dynamic t String'. Use
-- textInputEditedEvent with the output type to be updated with newly committed
-- text.
textInputWith
  :: forall r t m. OdinWidget r t m
  => Painter TextInputData IO
  -- ^ A custom painter to paint this text input widget.
  -> String
  -- ^ The placeholder text for the input. The input will stretch to accomodate
  -- the size of the text.
  -> TextInputCfg t
  -- ^ A configuration of update events.
  -> m (TextInputOutput t)
textInputWith painter txt cfg = do
  tvFresh       <- getFreshVar
  evMouseMotion <- getMouseMotionEvent
  evMouseButton <- getMouseButtonEvent
  evTextInput   <- getTextInputEvent
  evKeyboard    <- getKeyboardEvent

  let evUpdate = leftmost
        [ TextInputSetText            <$> cfg ^. setTextEvent
        , TextInputSetPainter         <$> cfg ^. setTextInputPainterEvent
        , TextInputSetPlaceholderText <$> cfg ^. setPlaceholderTextEvent
        , TextInputMouseMotion        <$> evMouseMotion
        , TextInputMouseButton        <$> evMouseButton
        , TextInputTextInput          <$> evTextInput
        , TextInputKeyboard           <$> evKeyboard
        ]
  initial <- liftIO $ do
    k      <- freshWith tvFresh
    paints <- mkPaintings painter "" txt
    return $ TII k TextInputStateUp "" "" txt ((-1)/0) painter paints
  dTII    <- accumM (foldTextInput tvFresh) initial evUpdate
  tellDyn $ pure . toWidget <$> dTII
  TextInputOutput <$> holdUniqDyn (tiiState <$> dTII)
                  <*> holdUniqDyn (tiiText  <$> dTII)


-- | Creates a text input for the user to input text. Returns an output type
-- containing a 'Dynamic t TextInputState' and 'Dynamic t String'. Use
-- textInputEditedEvent with the output type to be updated with newly committed
-- text.
textInput
  :: forall r t m. OdinWidget r t m
  => String
  -- ^ The placeholder text for the input. The input will stretch to accomodate
  -- the size of the text.
  -> TextInputCfg t
  -- ^ A configuration of update events.
  -> m (TextInputOutput t)
textInput txt cfg = do
  painter <- getTextInputPainter
  textInputWith painter txt cfg
