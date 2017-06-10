{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fbreak-on-exception              #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
module Main where




import           Control.Monad                  hiding (Layer)
import           Odin.Engine.Tiled
--------------------------------------------------------------------------------
import           TmxBrowser

defaultFont :: DefaultFont
defaultFont =
  DefaultFont $ fontDescriptor "assets/fonts/Inconsolata-Regular.ttf" 16

iconFont :: IconFont
iconFont = IconFont $ fontDescriptor "assets/fonts/FontAwesome.otf" 16

-- | A toon is a collection of animations for something/someone that moves around
-- in the game world.
data Toon = Toon { toonWalkingNorthAnimation :: Slot TiledAnimation
                 , toonWalkingEastAnimation  :: Slot TiledAnimation
                 , toonWalkingSouthAnimation :: Slot TiledAnimation
                 , toonWalkingWestAnimation  :: Slot TiledAnimation
                 }

data ToonPreview = ToonPreview { toonTitle :: Slot Text
                               , toonToon  :: Toon
                               }

parseToonPreviews
  :: OdinCont r => TiledMap -> VectorTileRenderer -> Eff r [ToonPreview]
parseToonPreviews tiledMap vtRend = sequence $ do
  let animations   = allAnimations tiledMap
      animationMap = map (\(sid, tid, a) -> (sid + tid, (sid, a))) animations
  toonLayer <- filterToonLayers tiledMap
  maybeToList $ do
    (name, (north, east, south, west)) <- parseCardinalGids toonLayer
    idsAndAnis <- mapM (`lookup` animationMap) [north, east, south, west]
    return $ do
      [n,e,s,w] <- forM idsAndAnis $ uncurry $ allocTiledAnimation vtRend
      DefaultFont font <- readDefaultFontDescriptor
      title <- slotText font black $ layerName toonLayer
      return ToonPreview { toonTitle = title
                         , toonToon  = Toon { toonWalkingNorthAnimation = n
                                            , toonWalkingEastAnimation  = e
                                            , toonWalkingSouthAnimation = s
                                            , toonWalkingWestAnimation  = w
                                            }
                         }

filterToonLayers :: TiledMap -> [Layer]
filterToonLayers tiledMap = do
  layer <- mapLayers tiledMap
  maybe [] (const $ return layer) $ lookup "ToonLayer" $ layerProperties layer

parseCardinalGids :: Layer -> Maybe (String, (Word32, Word32, Word32, Word32))
parseCardinalGids toonLayer = do
  cardinals <- parseCardinals toonLayer
  return (layerName toonLayer, cardinals)

parseCardinals :: Layer -> Maybe (Word32, Word32, Word32, Word32)
parseCardinals layer = do
  let mobjs  = case layerContents layer of
        LayerContentsObjects objs -> Just objs
        _                         -> Nothing
  objs <- mobjs
  let objMap = zip (map objectName objs) objs
  northGid <- objectGid =<< lookup (Just "North") objMap
  eastGid  <- objectGid =<< lookup (Just "East")  objMap
  southGid <- objectGid =<< lookup (Just "South") objMap
  westGid  <- objectGid =<< lookup (Just "West")  objMap
  return (northGid, eastGid, southGid, westGid)


newtype EditorConfig = EditorConfig { editorLastLevelFile :: Maybe FilePath}
  deriving (Show, Generic, Eq)

emptyEditorConfig = EditorConfig { editorLastLevelFile = Nothing }

instance FromJSON EditorConfig
instance ToJSON EditorConfig

showTimeFrom :: UTCTime -> UTCTime -> String
showTimeFrom now thn =
  let t = diffUTCTime now thn
  in if t < 60
       then show (floor t) ++ "s"
       else if t < 60*60
              then show (floor $ t/60) ++ "m"
              else show (floor $ t/(60*60)) ++ "h"

getTiledMapFile :: OdinCont r => Eff r FilePath
getTiledMapFile = autoRelease $ do
  DefaultFont font <- readDefaultFontDescriptor
  titleText <- slotText font black $ unlines
    [ "Welcome to the Odin level editor!"
    , "The first step in making an odin level is to open a *.tmx file."
    , ""
    , "If you have never created one of these, do so now by opening"
    , "the wonderful Tiled map editor and:"
    , ""
    , "* create a new file"
    , "* save it as \"level-one-or-something.tmx\""
    , ""
    , "When you're done and you have a *.tmx file, click the \"Okay, I'm ready\" button"
    ]
  V2 titleWidth titleHeight <- sizeOfText titleText
  let renderTitle =
        fix $ \rloop -> renderText titleText [move 16 16] >> next rloop

  okay <- slotButton buttonPainter "Okay, I'm ready."
  checkpoint "coldStart" $ fix $ \loop -> do
    renderText titleText [move 16 32]
    renderButton okay [move 16 $ 32 + titleHeight] >>= \case
      ButtonStateClicked -> return ()
      _                  -> next loop

  reslotText titleText font black $ unlines
    [ "Now navigate to the *.tmx file you'd like to import."]

  cwd  <- io getCurrentDirectory
  fix $ \loop -> raceEither renderTitle (runTmxBrowser cwd) >>= \case
    Left ()  -> next loop
    Right file -> return file

editor :: OdinCont r => Eff r ()
editor = autoRelease $ do
  odinConfigDir <- io $ getAppUserDataDirectory "odin"
  io $ createDirectoryIfMissing True odinConfigDir
  let previousUserFile = odinConfigDir </> "config"
  file <- checkpoint "getTmxFile" $ do
    editorConfig <- io (doesFileExist previousUserFile) >>= \case
      False -> return emptyEditorConfig
      True  -> io (decodeFileEither previousUserFile) >>= \case
        Left err  -> io (print err) >> return emptyEditorConfig
        Right cfg -> return cfg
    case editorLastLevelFile editorConfig of
      Nothing   -> getTiledMapFile
      Just file -> do
        DefaultFont font <- readDefaultFontDescriptor
        title <- slotText font black $ unlines
          [ "Would you like to continue working on"
          , show file ++ "?"
          ]
        yes           <- slotButton buttonPainter "Yes."
        V2 yesWidth _ <- sizeOfButton yes
        no            <- slotButton buttonPainter "No, I'll select a new one."
        raceAny
          [ nextForever $ renderText title [move 16 32]
          , fix $ \loop -> renderButton yes [move 16 60] >>= \case
              ButtonStateClicked -> return True
              _ -> next loop
          , fix $ \loop -> renderButton no [move (16 + yesWidth) 60] >>= \case
              ButtonStateClicked -> return False
              _ -> next loop
          ] >>= \case True  -> return file
                      False -> getTiledMapFile
  io $ encodeFile previousUserFile EditorConfig { editorLastLevelFile = Just file }
  -- Start an fsnotify manager that watches the tmx file for changes. When a
  -- change is found, insert the time it was updated.
  firstNow <- io getCurrentTime
  lastTimeUpdated <- slotVar firstNow
  mapFileUpdated  <- slotVar $ Just firstNow
  stopWatching <- io $ do
    mngr <- startManagerConf defaultConfig{confDebounce = Debounce 1}
    let pred (Added path _) = file == path
        pred _              = False
    watchDir mngr (takeDirectory file) pred $ \ev ->
      runM $ mapFileUpdated `is` Just (eventTime ev)
  DefaultFont font <- readDefaultFontDescriptor
  fileText <- slotText font black file
  timeText <- slotText font black "Last update: 0s"
  -- Make some slots to store our parsed toons out of
  toonsVar   <- slotVar []
  nextForever $ do
    now     <- io getCurrentTime
    lastUTC <- unslot lastTimeUpdated
    reslotText timeText font black $
      "Last update: " ++ showTimeFrom now lastUTC ++ " ago..."

    V2 _ h <- getWindowSize
    renderText fileText [move 0 $ h - 4]
    renderText timeText [move 0 $ h - 4 - 16]
    toonPreviews <- unslot toonsVar

    forM_ (zip [0..] toonPreviews) $ \(i, prev) -> do
      let origin = V2 16 16 + V2 0 (i * 48)
      renderText (toonTitle prev) [moveV2 origin]
      let north = toonWalkingNorthAnimation $ toonToon prev
          east  = toonWalkingEastAnimation  $ toonToon prev
          south = toonWalkingSouthAnimation $ toonToon prev
          west  = toonWalkingWestAnimation  $ toonToon prev
          westPos  = origin
          northPos = westPos  + V2 16  0
          eastPos  = northPos + V2 16  0
          southPos = northPos + V2 0  16
      renderTiledAnimation north [moveV2 northPos]
      advanceTiledAnimation north
      renderTiledAnimation east [moveV2 eastPos]
      advanceTiledAnimation east
      renderTiledAnimation south [moveV2 southPos]
      advanceTiledAnimation south
      renderTiledAnimation west [moveV2 westPos]
      advanceTiledAnimation west

    -- Update stuff when the tmx file changes!
    unslot mapFileUpdated >>= \case
      Just utc -> do
        lastTimeUpdated `is` utc
        mapFileUpdated  `is` Nothing
        -- Parse out our toons from the loaded tiledMap
        tiledMap         <- io $ unrelativizeImagePaths <$> loadMapFile file
        vecTiledRenderer <- createVectorTileRenderer tiledMap
        previews         <- parseToonPreviews tiledMap vecTiledRenderer
        toonsVar `is` previews
      _ -> return ()

main :: IO ()
main = do
  runM destroyAllocations
  backends <- getWindow
  runOdinIO backends defaultFont iconFont persistAllocations editor
