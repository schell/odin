module Odin.Renderer where

import Gelatin.SDL2

import Odin.Common

--renderStartScreen :: StartScreen
--------------------------------------------------------------------------------
-- Our big renderer
--------------------------------------------------------------------------------
data OdinRenderer = OdinRenderer
  { orStartScreen :: Transform -> IO ()
  , orGameOver :: Transform -> IO ()
  , orPic :: Picture () -> IO ()
  }
--------------------------------------------------------------------------------
-- Start screen
--------------------------------------------------------------------------------
startScreenPic :: OdinConfig -> Picture ()
startScreenPic cfg = do
  let fill = FillColor $ \(V2 _ y) -> V4 (abs y/70) (abs y/70) (abs y/70) 1
      text = letters 128 64 "Odin"
      logo = withFill fill $ withFont (ocFancyFont cfg) $ withFill fill text
      instructions = withFont (ocLegibleFont cfg) $ withFill (solid grey) $
                       letters 128 16 "Press any key to play"
  move (V2 0 64) logo
  move (V2 0 68) instructions

gameOverPic :: OdinConfig -> Picture ()
gameOverPic cfg = do
  move (V2 0 64) $ withFill (solid red) $ withFont (ocFancyFont cfg) $
    letters 128 64 "Game Over"
  move (V2 0 68) $ withFill (solid grey) $ withFont (ocLegibleFont cfg) $
    letters 128 16 "Press any key to play again :)"

makeStartScreen :: Rez -> OdinConfig -> IO (Transform -> IO ())
makeStartScreen rez cfg =
  snd <$> compileRenderer rez (startScreenPic cfg)

makeGameOverScreen :: Rez -> OdinConfig -> IO (Transform -> IO ())
makeGameOverScreen rez cfg =
  snd <$> compileRenderer rez (gameOverPic cfg)

makePicScreen :: Rez -> IO (Picture () -> IO ())
makePicScreen rez = return $ \pic -> do
  rc <- compileRenderer rez pic
  snd rc mempty
  fst rc

-- | Create the entire sum renderer.
makeOdinRenderer :: Rez -> OdinConfig -> IO OdinRenderer
makeOdinRenderer rez cfg =
  OdinRenderer <$> (makeStartScreen rez cfg)
               <*> (makeGameOverScreen rez cfg)
               <*> (makePicScreen rez)

-- | Render the game.
renderOdin :: OdinRenderer -> Odin -> IO ()
renderOdin o (OdinStart _) = orStartScreen o mempty
renderOdin o (OdinGameOver _) = orGameOver o mempty
renderOdin o (OdinPic p) = orPic o p
