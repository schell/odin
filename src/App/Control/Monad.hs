{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App.Control.Monad where

import           Gelatin.SDL2
import           SDL hiding (get, Cursor)
import qualified SDL.Raw.Types as Raw
import           Control.Varying
import           Control.Monad.Trans.Class (lift, MonadTrans)
import           Control.Monad.Trans.RWS.Strict
import           Data.Time.Clock
import           Data.Map.Strict (Map)
import           Data.Text (Text)

newtype Uid = Uid { unUid :: Int } deriving (Show, Eq, Ord, Enum, Num)

data LogLevel = LogLevelInfo
              | LogLevelWarn
              | LogLevelError
              deriving (Show, Eq, Ord, Bounded, Enum)

data CursorType = CursorHand
                deriving (Show, Eq, Ord)

data CursorCmd = CursorPush CursorType
               | CursorPop CursorType
               deriving (Show, Eq, Ord)

type CursorStack = Map CursorType (Int, Raw.Cursor)

data Action = ActionNone
            | ActionLog LogLevel String
            | ActionSetCursor CursorCmd
            | ActionSetTextEditing Bool

data Fonts = Fonts { fontFancy   :: FontData
                   , fontLegible :: FontData
                   , fontIcon    :: FontData
                   }

data AppData = AppData
  { appNextId :: Uid
  , appLogic  :: VarT Effect AppEvent (Picture GLuint ())
  , appCache  :: Cache IO PictureTransform
  , appUTC    :: UTCTime
  , appFonts  :: Fonts
  , appRez    :: Rez
  , appCursor :: CursorStack
  }

data ReadData = ReadData { rdWindowSize :: V2 Int
                         , rdCursorPos  :: V2 Int
                         , rdFonts      :: Fonts
                         }

type StateData = Uid

type Effect = RWST ReadData [Action] StateData IO

data AppEvent = AppEventNone
              | AppEventTime Float
              | AppEventFrame
              | AppEventKey { keyMotion :: InputMotion
                            , keyRepeat :: Bool
                            , keySym    :: Keysym
                            }
              | AppEventTextInput Text
              | AppEventDrop FilePath
              | AppEventMouseMotion (V2 Int) (V2 Int)
              | AppEventMouseButton MouseButton InputMotion (V2 Int)
              | AppEventWheel (V2 Int)
              | AppEventOther EventPayload
              | AppQuit
              deriving (Show, Eq)

data Delta = DeltaTime Float
           | DeltaFrames Int

type AppSequence = SplineT AppEvent (Picture GLuint ()) Effect
type AppSignal = VarT Effect AppEvent

--------------------------------------------------------------------------------
-- Empty AppData
--------------------------------------------------------------------------------
appData :: Fonts -> Rez -> UTCTime -> AppData
appData fnts rez utc =
  AppData { appNextId = 0
          , appLogic  = pure blank
          , appUTC    = utc
          , appCache  = mempty
          , appFonts  = fnts
          , appRez    = rez
          , appCursor = mempty
          }
--------------------------------------------------------------------------------
-- Fresh new Uids
--------------------------------------------------------------------------------
fresh :: (Monad m, Monoid w) => RWST r w Uid m Uid
fresh = do
  uid <- get
  modify (+1)
  return uid
--------------------------------------------------------------------------------
-- Read only app state
--------------------------------------------------------------------------------
-- | Read our different fonts.
getIconFont,getFancyFont,getLegibleFont :: (Monad m, Monoid w)
                                        => RWST ReadData w s m FontData
getIconFont    = asks (fontIcon    . rdFonts)
getFancyFont   = asks (fontFancy   . rdFonts)
getLegibleFont = asks (fontLegible . rdFonts)

-- | Read the current mouse position
getMousePosition :: (Monad m, Monoid w) => RWST ReadData w s m (V2 Int)
getMousePosition = asks rdCursorPos
--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------
infoStr :: (Monad m, MonadTrans t) => String -> t (RWST r [Action] s m) ()
infoStr str = lift $ tell [ActionLog LogLevelInfo str]
--------------------------------------------------------------------------------
-- Setting the cursor
--------------------------------------------------------------------------------
pushCursor :: (Monad m, MonadTrans t)
            => CursorType -> t (RWST r [Action] s m) ()
pushCursor c = lift $ tell [ActionSetCursor $ CursorPush c]

popCursor :: (Monad m, MonadTrans t)
          => CursorType -> t (RWST r [Action] s m) ()
popCursor c = lift $ tell [ActionSetCursor $ CursorPop c]
--------------------------------------------------------------------------------
-- Text editing
--------------------------------------------------------------------------------
startTextEditing :: (Monad m, MonadTrans t) => t (RWST r [Action] s m) ()
startTextEditing = lift $ tell [ActionSetTextEditing True]

stopTextEditing :: (Monad m, MonadTrans t) => t (RWST r [Action] s m) ()
stopTextEditing = lift $ tell [ActionSetTextEditing False]
