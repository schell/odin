module Odin.Renderer where

import Gelatin.SDL2
import Odin.Common
import qualified Data.IntMap as IM

--------------------------------------------------------------------------------
-- Our big renderer
--------------------------------------------------------------------------------
odinPic :: OdinConfig -> Odin -> Picture GLuint ()
odinPic cfg (OdinStart s) = startScreen cfg s
odinPic cfg (OdinRun b) = boardScreen cfg b
odinPic cfg (OdinEnd s) = gameOver cfg s
odinPic cfg (OdinPic f) = f cfg
--------------------------------------------------------------------------------
-- Screens
--------------------------------------------------------------------------------
startScreen :: OdinConfig -> StartScreen -> Picture GLuint ()
startScreen cfg _ = do
  let fill = FillColor $ \(V2 _ y) -> V4 (abs y/70) (abs y/70) (abs y/70) 1
      logo = letters $ filled (Name 0) (ocFancyFont cfg) 128 64
               "Odin" fill
      instructions = letters $ filled (Name 0) (ocLegibleFont cfg) 128 16
                       "Press any key to play" $ solid grey
  move (V2 0 64) $ draw logo
  move (V2 0 68) $ draw instructions

drawInterface :: OdinConfig -> Interface -> Picture GLuint ()
drawInterface cfg Interface = move (V2 10 16) $ withLetters $
  filled (Name 0) (ocLegibleFont cfg) 128 16 "The interface" $ solid white

drawBoardMap :: OdinConfig -> BoardMap -> V2 Int -> Picture GLuint ()
drawBoardMap cfg (BoardMap m) size = withColor $ do
  let V2 ww wh = fromIntegral <$> size
  rectangle (V2 10 16) (V2 (ww - 10) (wh - 10)) $ const white
  rectangle (V2 14 20) (V2 (ww - 14) (wh - 14)) $ const black

boardScreen :: OdinConfig -> Board -> Picture GLuint ()
boardScreen cfg (Board iface bmap size) = do
  drawInterface cfg iface
  drawBoardMap cfg bmap size

gameOver :: OdinConfig -> GameOverScreen -> Picture GLuint ()
gameOver cfg _ = do
  move (V2 0 64) $ withLetters $ filled (Name 0) (ocFancyFont cfg) 128 64
    "Game Over" $ solid red
  move (V2 0 68) $ withLetters $ filled (Name 0) (ocLegibleFont cfg) 128 16
    "Press any key to play again :)" $ solid grey
