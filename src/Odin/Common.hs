module Odin.Common where

import Gelatin.SDL2 hiding (Event)
import System.FilePath
import System.Directory
import System.Exit
import Control.Varying
import Control.Monad (void)

odinConfig :: IO OdinConfig
odinConfig = do
-- Get our fonts
    assets <- (</> "assets") <$> getCurrentDirectory
    -- Load our header font
    let fonts = assets </> "fonts"
    ed <- loadFont $ fonts </> "Deutsch.ttf"
    eh <- loadFont $ fonts </> "Hack-Regular.ttf"
    (deutsch, hack) <- case (,) <$> ed <*> eh of
        Left err -> do print err
                       exitFailure
        Right fs -> return fs
    -- Define our tileset
    let images = assets </> "images"
        tileset  = TileSet { tsInterface = images </> "Interface.png"
                           , tsTerrain = images </> "Terrain.png"
                           , tsAvatar = images </> "Avatar.png"
                           }
    return OdinConfig { ocFancyFont = deutsch
                 , ocLegibleFont = hack
                 , ocTileSet = tileset
                 }


_untilEvent_ :: Monad m => VarT m a b -> VarT m a (Event c) -> SplineT a b m ()
_untilEvent_ = (void .) . untilEvent

isQuit :: Keysym -> Bool
isQuit (Keysym (Scancode 20) (Keycode 113) m) = any ($ m)
    [ keyModifierLeftCtrl
    , keyModifierRightCtrl
    , keyModifierLeftGUI
    , keyModifierRightGUI
    ]
isQuit _ = False
--------------------------------------------------------------------------------
-- A tileset for our renderer
--------------------------------------------------------------------------------
data TileSet = TileSet { tsInterface :: FilePath
                       , tsTerrain   :: FilePath
                       , tsAvatar    :: FilePath
                       }
--------------------------------------------------------------------------------
-- A configuration type for displaying Odin
--------------------------------------------------------------------------------
data OdinConfig = OdinConfig { ocFancyFont :: FontData
                             , ocLegibleFont :: FontData
                             , ocTileSet :: TileSet
                             }
--------------------------------------------------------------------------------
-- The startup screen
--------------------------------------------------------------------------------
data StartScreen = StartScreenWait
--------------------------------------------------------------------------------
-- The game over screen
--------------------------------------------------------------------------------
data GameOverScreen = GameOverScreen
--------------------------------------------------------------------------------
-- The entire sum type
--------------------------------------------------------------------------------
data Odin = OdinStart StartScreen
          | OdinGameOver GameOverScreen
          | OdinPic (Picture ())

