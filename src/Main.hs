module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (first))
import Data.Bits (Bits (shift))
import Data.Function ((&))
import qualified Data.Map.Strict as M
import Data.Monoid (Any (Any, getAny))
import FRP.BearRiver hiding (first)
import Lightarrow hiding (rotation, _rotation)
import Linear (V2 (V2))
import Linear.Affine (Affine ((.+^)), Point (P))
import System.Lightarrow.Run (Lightarrow, run)
import System.Random.TF.Gen (RandomGen, seedTFGen)
import System.Random.TF.Instances (Random (random), randomEnum)

import Tetris.Action
import Tetris.Grid

main :: IO ()
main = do
    putStrLn ""
    putStrLn "      ┌──────────────────────┐"
    putStrLn "      │     Tetris in LA     │"
    putStrLn "      │    by Alex Stuart    │"
    putStrLn "      └──────────────────────┘"
    putStrLn ""
    putStrLn "Game start!"
    run game

game :: Lightarrow () ()
game = do
    liftIO (putStrLn "Setting up platform...")
    r <- setup
    liftIO (putStrLn "...done.")
    let boot = return mempty
        inp = input r
        out = output r
    liftIO (putStrLn "Starting reactimation.")
    reactimate boot inp out (mainAutomaton r)
    return ()

input :: (SensePlatform m, TimingPlatform m) =>
    Resources m
    -> p
    -> m (DTime, Maybe (Sensation m))
input r _ = do
    s <- sense r
    dt <- timeStep r ()
    return (dt, Just s)

output :: (ActuatePlatform m, KeyboardPlatform m) =>
    Resources m
    -> p
    -> Actuation m
    -> m Bool
output r _ a = do
    actuate r a
    s <- sense r
    return (keyPressed escapeKey s)

mainAutomaton ::
    Resources (Lightarrow ())
    -> SF
        (Lightarrow ())
        (Sensation (Lightarrow ()))
        (Actuation (Lightarrow ()))
mainAutomaton r = runTask fullGame undefined
  where
    fullGame = do
        s10 <- liftPF (getRandom r)
        s11 <- liftPF (getRandom r)
        s20 <- liftPF (getRandom r)
        s21 <- liftPF (getRandom r)
        s30 <- liftPF (getRandom r)
        s31 <- liftPF (getRandom r)
        s40 <- liftPF (getRandom r)
        s41 <- liftPF (getRandom r)
        let s1 = fromIntegral s11 `shift` 32 + fromIntegral s10
            s2 = fromIntegral s21 `shift` 32 + fromIntegral s20
            s3 = fromIntegral s31 `shift` 32 + fromIntegral s30
            s4 = fromIntegral s41 `shift` 32 + fromIntegral s40
        nextPiece 1 M.empty (seedTFGen (s1, s2, s3, s4))

nextPiece :: RandomGen g =>
    Double
    -> M.Map (Point V2 Int) Color
    -> g
    -> Task
        (Sensation (Lightarrow ()))
        (Actuation (Lightarrow ()))
        (Lightarrow ())
        ()
nextPiece speed obstacles g0 = do
    points <- onlyUntil settled (pieceMoves shape speed obstacles)
                & mapTask (>>> arr (first (drawTiles . (obstacles <>) . tiles)))
    let clearedObstacles = clearFullRows (obstacles <> tiles points)
    if M.member (P (V2 0 8)) clearedObstacles
        then return ()
        else nextPiece (speed + 0.1) clearedObstacles g2
  where
    settled = proc (_in, tiles) -> do
        done <- iEdge False -< getAny (foldMap (Any . occupied) tiles)
        returnA -< done `tag` fmap (.+^ V2 0 1) tiles
    tiles points = M.fromList (points `zip` repeat color)
    occupied p@(P (V2 _ y)) = y < (-10) || M.member p obstacles
    (shape, g1) = randomEnum (minBound, maxBound) g0
    (color, g2) = first ((colors !!) . (`mod` length colors)) (random g1)

colors :: [Color]
colors =
    [ Blue
    , Cyan
    , Gray
    , Green
    , Magenta
    , Red
    , White
    , Yellow
    ]