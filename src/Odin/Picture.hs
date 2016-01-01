grid :: Float -> Float -> Float -> Float -> Picture ()
grid w h inc maxY = do
    withStroke [StrokeColor grey, StrokeFeather 0.5, StrokeWidth 1] $ do
        forM_ [0,inc .. w] $ \x -> polyline [V2 x 0, V2 x h]
        forM_ [0,inc .. h] $ \y -> polyline [V2 0 y, V2 w y]
    withStroke [StrokeColor lightGrey, StrokeFeather 0.5, StrokeWidth 1] $ do
        forM_ [0,inc*2 .. w] $ \x -> polyline [V2 x 0, V2 x h]
        forM_ [0,inc*2 .. h] $ \y -> polyline [V2 0 y, V2 w y]
    let inset = 10
        size  = 12
    forM_ [0, inc*4 .. h] $ \y -> do
        withStroke [StrokeColor pink, StrokeFeather 0, StrokeWidth 1] $
            polyline [V2 0 y, V2 10 y]
        withFill (solid pink) $
            withTransform (Transform (V2 (inset + 2) y) 1 0) $
                letters (FontDescriptor "Arial" $ FontStyle False False)
                        size (show $ y/h * maxY)

graph :: (MonadIO m, Monoid w) => Var (RWST ReadData w s m) a (Picture ())
graph = samples ~> varM picf
    where samples = linesAndWindow ~> accumulate (\acc (V2 w _,lns) -> take (floor w) $ reverse lns ++ acc) []

          linesAndWindow = (,) <$> windowSize
                               <*> fileLines "samples.txt"
          f :: String -> IO (Maybe Float)
          f ln = case fmap fst $ listToMaybe $ reads ln :: Maybe Int of
                     Nothing -> do putStrLn $ "could not read " ++ show ln
                                   return Nothing
                     Just i -> return $ Just $ realToFrac i
          picf :: (MonadIO m, Monoid w) => [String] -> (RWST ReadData w s m) (Picture ())
          picf lns = do
              V2 w h <- asks _readWindowSize
              lns'   <- liftIO $ catMaybes <$> mapM f lns
              return $ do
                  let toPoint x y = V2 x $ h * y/1024
                  grid w h 5 1024
                  withStroke [StrokeColor white, StrokeFeather 1, StrokeWidth 3] $
                      polyline $ zipWith toPoint [0..] $ reverse lns'
                  withTransform (Transform (V2 (w - 100) h) 1 0) $
                      withFill (solid white) $
                          letters (FontDescriptor "Arial" $ FontStyle True False)
                                  32 $ show $ head lns'

--field :: Maybe TextField -> Odin ()
--field mtxt = textField mempty mtxt >>= field . Just
--
--fields :: [String] -> [Maybe String -> Odin String]
--fields ss = zipWith f ss [(0 :: Int) ..]
--    where f s i mt = g <$> textField vt (Just $ mutate defaultTextField $ do
--                            textFieldLabel._2.plainTextString .= s
--                            textFieldInput.textInputText._2.plainTextString .= fromMaybe "" mt)
--                                where vt = pure t
--                                      t = Transform (V2 0 (32* fromIntegral i)) 1 0
--          txt = textFieldInput.textInputText._2.plainTextString
--          g = (^. txt)
