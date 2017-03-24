{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}
module DevelMain where

import           Control.Monad   (void)
import           Data.Function   (fix)
import           Odin.Engine.Eff
import           Odin.Engine.GUI

defaultFont :: DefaultFont
defaultFont = DefaultFont $ fontDescriptor "../assets/fonts/KMKDSP__.ttf" 16

iconFont :: IconFont
iconFont = IconFont $ fontDescriptor "../assets/fonts/FontAwesome.otf" 16

getBackends :: IO (Either String SDL2Backends)
getBackends =
  runEitherT (startupSDL2Backends 800 600 "Odin Engine Development" True)

withBackends :: (SDL2Backends -> IO a) -> IO ()
withBackends f = getBackends >>= \case
  Left err       -> putStrLn err
  Right backends -> void $ f backends

runner :: Eff (Next ': OdinFx) ()
runner = do
  DefaultFont font <- readDefaultFontDescriptor
  text <- slotText font black "Here is some text"
  fix $ \loop -> do
    renderText text [move 0 16]
    next loop

update :: IO ()
update = withBackends $ \(SDL2Backends v2v4 v2v2) -> do
  t <- runM newTime
  let fx :: Eff OdinFx (Status OdinFx () () ())
      fx = runC runner
      looper :: Eff OdinFx ()
      looper = loopFrame fx
  void $ runOdinFx v2v2 v2v4 defaultFont iconFont 0 t mempty emptyUi (Allocated []) looper
