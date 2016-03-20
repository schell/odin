module Odin.Common where

import Gelatin.SDL2 hiding (Event)
import System.FilePath
import System.Directory
import System.Exit
import Data.Map (Map)

loadTileSet :: FilePath -> FilePath -> FilePath -> IO (Either String TileSet)
loadTileSet iface terrain avatar = do
  ma <- loadImageAsTexture iface
  mb <- loadImageAsTexture terrain
  mc <- loadImageAsTexture avatar
  return $ case TileSet <$> ma <*> mb <*> mc of
    Nothing -> Left "Could not load tileset."
    Just ts -> Right ts

odinConfig :: IO OdinConfig
odinConfig = do
-- Get our fonts
    assets <- (</> "assets") <$> getCurrentDirectory
    -- Load our header font
    let fonts = assets </> "fonts"
    ed <- loadFont $ fonts </> "Deutsch.ttf"
    eh <- loadFont $ fonts </> "Hack-Regular.ttf"
    ef <- loadFont $ fonts </> "fontawesome-webfont.ttf"
    (deutsch, hack, awe) <- case (,,) <$> ed <*> eh <*> ef of
        Left err -> do print err
                       exitFailure
        Right fs -> return fs
    -- Define our tileset
    let images = assets </> "images" </> "oryx_roguelike_2.0"
        iface = images </> "Interface.png"
        terrain = images </> "Terrain.png"
        avatar = images </> "Avatar.png"
    tileset <- do ets <- loadTileSet iface terrain avatar
                  case ets of
                    Left err -> do putStrLn err
                                   exitFailure
                    Right ts -> return ts

    return OdinConfig { ocFancyFont = deutsch
                 , ocLegibleFont = hack
                 , ocIconFont = awe
                 , ocTileSet = tileset
                 }

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
data TileSet = TileSet { tsInterface :: GLuint
                       , tsTerrain   :: GLuint
                       , tsAvatar    :: GLuint
                       }
--------------------------------------------------------------------------------
-- A configuration type for displaying Odin
--------------------------------------------------------------------------------
data OdinConfig = OdinConfig { ocFancyFont   :: FontData
                             , ocLegibleFont :: FontData
                             , ocIconFont    :: FontData
                             , ocTileSet     :: TileSet
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
-- The character select screen
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- The main game
--------------------------------------------------------------------------------
data Interface = Interface
data Tile = TileStone
          | TileWater

newtype BoardMap = BoardMap { unBoardMap :: Map (Int,Int) Tile }

instance Monoid BoardMap where
  mempty = BoardMap mempty
  mappend (BoardMap a) (BoardMap b) = BoardMap $ a `mappend` b

data Board = Board { bInterface :: Interface
                   , bMap       :: BoardMap
                   , bSize      :: V2 Int
                   }
--------------------------------------------------------------------------------
-- The entire sum type
--------------------------------------------------------------------------------
data Odin = OdinStart StartScreen
          | OdinRun Board
          | OdinEnd GameOverScreen
          | OdinPic (OdinConfig -> Picture GLuint ())

