{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fbreak-on-exception              #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
module TmxBrowser where



import           Control.Monad    (foldM, forM)
import           Data.List        (isInfixOf, zip4)
import           Data.Maybe       (catMaybes)
import           Gelatin.SDL2
import           System.Directory
import           System.FilePath
--------------------------------------------------------------------------------
import           Odin.Engine
import           Odin.Engine.GUI

getFilteredDirectoryContents
  :: Member IO r
  => [String]
  -> FilePath
  -> Eff r [FilePath]
getFilteredDirectoryContents strs cwd = do
  files  <- io $ getDirectoryContents cwd
  mfiles <- forM files $ \file -> do
    isDir <- io $ doesDirectoryExist $ cwd </> file
    return $ if (isDir || any (`isInfixOf` file) strs) && file /= "."
      then Just file
      else Nothing
  return $ catMaybes mfiles

foldMFor :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
foldMFor b ta f = foldM f b ta

runTmxBrowser
  :: OdinCont r
  => FilePath
  -> Eff r FilePath
runTmxBrowser dir = autoRelease $ do
  buttonStuff@(_, _, _, panelHeight, maxWidth)
    <- getFilesButtonsSizes dir
  panel <- do
    let panelSize   = 400
        contentSize = mkContentSize maxWidth panelHeight
    slotPanel "Select a tmx file to work on:" panelSize contentSize
  loop dir panel buttonStuff

  where mkContentSize maxWidth panelHeight =
          20 + (floor <$> V2 (maxWidth + 4) panelHeight)

        getFilesButtonsSizes file = do
          files   <- getFilteredDirectoryContents [".tmx"] file
          buttons <- mapM (slotButton buttonPainter) files
          (sizes, panelHeight, maxWidth) <- foldMFor ([], 0, 0) buttons $
            \(sizes, panelHeight, maxWidth) button -> do
              size@(V2 w h) <- sizeOfButton button
              return (sizes ++ [size], panelHeight + h, max maxWidth w)
          return (files, buttons, sizes, panelHeight, maxWidth)

        renderTmxBrowser cwd panel files buttons sizes = do
          (mClickedOnFile, _) <- renderPanel panel [move 2 32] $ \scrollOffset -> do
              let buttonStuff = zip4 [0..] files buttons sizes
              foldMFor Nothing buttonStuff $ curry $ \case
                (Just file, _) -> return $ Just file
                (Nothing, (i, file, btn, V2 _ h)) ->
                  renderButton btn [ move 20 $ 20 + i * h
                                   , moveV2 $ fromIntegral <$> scrollOffset
                                   ] >>= \case
                    ButtonStateClicked -> return $ Just $ case file of
                      ".." -> takeDirectory cwd
                      _    -> cwd </> file

                    _ -> return Nothing
          return mClickedOnFile

        loop cwd panel stuff@(files, buttons, sizes, _, _) = do
          mClickedOnFile <- renderTmxBrowser cwd panel files buttons sizes
          case mClickedOnFile of
            Nothing   -> next $ loop cwd panel stuff
            Just file -> io (doesFileExist file) >>= \case
              True  -> next $ return file
              False -> do
                newStuff@(_,_,_, panelHeight, maxWidth) <-
                  getFilesButtonsSizes file
                Panel{..} <- unslot panel
                let contentSize = mkContentSize maxWidth panelHeight
                resizePaneContent pPane contentSize
                offsetPane pPane 0
                next $ loop file panel newStuff
