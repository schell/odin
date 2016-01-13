{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Odin.Control (
    --module C,
    network
) where

--import Odin.Control.TextField
--import Odin.Control.TextForm
--import Odin.Control.Button
import Jello.GLFW
import Odin.Control.Button
import Odin.Control.Common as C
import Control.Monad.IO.Class
import Control.Monad.Trans
import System.Exit

rect :: Picture f ()
rect = withFill (solid grey) $ rectangle 100

errorScreen sz = withTransform (Transform (sz/2) 1 0) $
    withFill (solid red) $ rectangle sz

boundedPic textPic = do
    let fill = withFill (solid white) textPic
        stroke = withStroke [StrokeWidth 1, StrokeFeather 0, StrokeColor white] $
                    textPic
        bounds = withTransform (Transform (V2 (w/2) (-h/2)) 1 0) $
                    withStroke [StrokeWidth 1, StrokeFeather 0, StrokeColor red] $
                        rectangle sz
        sz@(V2 w h) = pictureSize textPic
    bounds
    fill
    withTransform (Transform (V2 (w+2) 0) 1 0) $ do
        bounds
        stroke

next :: (Applicative m, Monad m, Monoid b) => Var m a (Event b) -> Spline a b m b
next = spline mempty

next_ :: (Applicative m, Monad m, Monoid b) => Var m a (Event b) -> Spline a b m ()
next_ v = do _ <- next v
             return ()

linearRotation :: MonadIO m => Float -> Var m a Float
linearRotation t = time ~> (execSpline 0 $ rspline)
    where rspline :: MonadIO m => Spline Float Float m ()
          rspline = tween linear 0 (2*pi) t >> rspline

spinner :: MonadIO m => Var m a (Picture f ())
spinner =
    rotate <$> linearRotation 2
           <*> (pure $ withStroke [ StrokeWidth 4, StrokeFeather 1
                                  , StrokeColors [white `alpha` 0, white]
                                  ] $
                   arc 10 0 (2*pi - pi/4))

getFontsOrDie :: App (Font,Font,FontCache)
getFontsOrDie = do
    efc <- snd <$> spinner `untilEvent` newFontCacheLoaded
    fc  <- case efc of
               Left exc -> liftIO $ do print exc
                                       exitFailure
               Right fc -> return fc
    let Just arialp = findFontInCache fc arialDescriptor
        Just hackp = findFontInCache fc hackDescriptor
    Right arial <- liftIO $ loadFontFile arialp
    Right hack  <- liftIO $ loadFontFile hackp
    return (arial,hack,fc)

arialDescriptor :: FontDescriptor
arialDescriptor = FontDescriptor "Arial" $ FontStyle False False

hackDescriptor :: FontDescriptor
hackDescriptor = FontDescriptor "Hack" $ FontStyle False False

mapApp :: (Picture Font () -> Picture Font ()) -> App a -> App a
mapApp f = mapOutput (pure f)

rendererReadOut :: Font -> Var (RWST (ReadData UserData) () s IO) a GLPic
rendererReadOut fnt = readout <$> windowSize <*> currentNumberOfRenderers
    where readout (V2 _ h) n =
            let pic = withFill (solid white) $ withFont fnt $
                        letters 16 $ "Renderers: " ++ show n
            in move (V2 0 h) pic

network :: App ()
network = do
    sz <- lift $ asks readWindowSize

    (arial,hack,_) <- mapApp (move $ sz/2) getFontsOrDie

    let arialText = withFont arial $ letters 16 "This is Arial."
        hackText = withFont hack $ letters 16 "This is Hack."

        text = do move 100 $ boundedPic arialText
                  move (V2 100 120) $ boundedPic hackText
                  let r = 10
                      p = 2 + (V2 r r)
                  move p $ withFill (solid pink) $ circle 10
                  move (sz/2) $ withStroke [ StrokeWidth 4
                                           , StrokeFeather 1
                                           , StrokeColors [cyan,magenta]
                                           ] $
                    do (circle 100)
                       let erc = arc (V2 100 50) (pi - pi/8) (2*pi - pi/8)
                       move 50 erc
        textAndReadout = (text >>) <$> rendererReadOut hack

    let loop = do (pic,_) <- textAndReadout `untilEvent` enter
                  _ <- output pic
                  (pic',_) <- rendererReadOut hack `untilEvent` enter
                  _ <- output pic'
                  loop


    loop

    liftIO exitSuccess
