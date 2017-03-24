{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
import           Control.Monad              (when)
import           Control.Monad.Freer.Writer
import           Data.Function              (fix)
import           Test.Hspec

import           Odin.Engine.Eff
import           Odin.Engine.GUI
import           Odin.Engine.Slots

twoComp :: (Member (Writer [String]) r, Member (State Int) r, Member Next r) => Eff r ()
twoComp = do
  tell ["i3"]
  fix $ \loop -> do
    n :: Int <- get
    tell ["r3-" ++ show n]
    when (n < 3) $ next loop
  tell ["d3"]

fourComp :: (Member (Writer [String]) r, Member (State Int) r, Member Next r) => Eff r ()
fourComp = do
  tell ["i4"]
  fix $ \loop -> do
    n :: Int <- get
    tell ["r4-" ++ show n]
    put $ n + 1
    when (n < 4) $ next loop
  tell ["d4"]

type Fx = '[Next, State Int, Writer [String]]
type Fy = '[State Int, Writer [String]]

bothComps :: (Member Next r, Member (State Int) r, Member (Writer [String]) r) => Eff r ()
bothComps = do
  raceEither twoComp fourComp >>= \case
    Left ()  -> tell ["!3"]
    Right () -> tell ["!4"]
  fix $ \loop -> do
    tell ["10-loop"]
    n :: Int <- get
    when (n < 10) $ do
      modify (+(10 :: Int))
      next loop

nestComps
  :: Members '[Next, State Int, Writer [String]] r
  => Eff r (Status r () () w)
  -> Eff r w
nestComps comp = do
  tell ["in"]
  status <- comp
  n :: Int <- get
  tell ["n-" ++ show n]
  case status of
    Done w -> do
      tell ["!n"]
      return w
    Continue () f -> nestComps $ f ()

runFx :: Eff Fx a -> ((Status Fy () () a, Int), [String])
runFx = run . runWriter . flip runState (0 :: Int) . runC

runFy :: Int -> Eff Fy (Status Fy () () a) -> ((Status Fy () () a, Int), [String])
runFy s = run . runWriter . flip runState s

exhaust' :: Int -> [String] -> ((Status Fy () () a, Int), [String]) -> [String]
exhaust' n ys = \case
  ((Done _, s), zs) -> concat [ys, zs, ["e" ++ show s]]
  ((Continue () ff, s), zs) ->
    let yys = concat [ys,zs,["c" ++ show n]]
    in exhaust' (n + 1) yys $ runFy s $ ff ()

exhaust :: ((Status Fy () () a, Int), [String]) -> [String]
exhaust = exhaust' 0 []

main :: IO ()
main = hspec $ do
  describe "Frame by frame continuations" $
    it "work, and eventually terminate" $ do
      unwords (exhaust $ runFx (bothComps :: Eff Fx ()))
        `shouldBe`
        "i3 r3-0 i4 r4-0 c0 r3-1 r4-1 c1 r3-2 r4-2 c2 r3-3 d3 !3 10-loop c3 10-loop e13"
      unwords (exhaust $ runFx (nestComps $ interposeC bothComps :: Eff Fx ()))
        `shouldBe`
        "in i3 r3-0 i4 r4-0 n-1 in r3-1 r4-1 n-2 in r3-2 r4-2 n-3 in r3-3 d3 !3 10-loop n-13 in 10-loop n-13 !n e13"
