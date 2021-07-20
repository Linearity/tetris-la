{-# LANGUAGE LambdaCase #-}
module Tetris.Action where

import Control.Applicative (Alternative ((<|>)), Applicative (liftA2))
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.State.Strict (MonadState (get, put), StateT, forever, gets)
import qualified Data.Map.Strict as M
import Data.Monoid (Any (Any, getAny))
import FRP.BearRiver
import Lightarrow hiding (_rotation)
import Linear (V2 (V2))
import Linear.Affine (Affine ((.+^), (.-^)), Point (P))
import Optics (modifying, (%~), (&))
import Tetris.Piece (PieceShape, PieceState (PieceState, speed), points, _location, _rotation)
import System.Lightarrow.Run (Lightarrow)

-- | The whole motion of the current piece
pieceMoves :: Num a =>
    PieceShape
    -> Double
    -> M.Map (Point V2 Int) b
    -> Task
        (Sensation (Lightarrow ()))
        [Point V2 Int]
        (Lightarrow ())
        [Point V2 a]
pieceMoves shape speed obstacles = do
    pieceFalls `mix` pieceSlides obstacles `mix` pieceRotates obstacles
        & mapTask (execStateSF state0)
            . mapTask
                ( >>>
                    arrM
                        ( \case
                            Left _ -> Left . points <$> get
                            Right c -> return (Right c)
                        )
                )
  where
    mix t1 t2 = unVoice (liftA2 (const (const [P (V2 0 0)])) (Voice t1) (Voice t2))
    state0 = PieceState (P (V2 0 8)) 0 shape speed

-- | The vertical motion of the current piece
pieceFalls ::
    Task
        (Sensation (Lightarrow ()))
        ()
        (StateT PieceState (Lightarrow ()))
        b
pieceFalls = do
    forever
        ( do
            sp <- gets speed
            interval (1 / sp) (constant mempty)
            modifying _location (.-^ V2 0 1)
        )
        & onlyUntil (edge <<< arr (keyPressed arrowDownKey . fst))
    always (constM (ask >>= \dt -> modifying _location (.-^ V2 0 (20 * dt))))
        & onlyUntil (iEdge False <<< arr (not . keyPressed arrowDownKey . fst))
    pieceFalls

-- | The horizontal motion of the current piece
pieceSlides ::
    M.Map (Point V2 Int) a
    -> Task (Sensation (Lightarrow ())) () (StateT PieceState (Lightarrow ())) c
pieceSlides obstacles = always $
    proc s -> do
        left <- edge -< keyPressed arrowLeftKey s
        right <- edge -< keyPressed arrowRightKey s
        piece <- constM get -< ()
        let pieceL = piece & _location %~ (.-^ V2 1 0)
            pieceR = piece & _location %~ (.+^ V2 1 0)
            blockedL = getAny (foldMap occupied (points pieceL))
            blockedR = getAny (foldMap occupied (points pieceR))
            new =
                gate left (not blockedL) `tag` pieceL
                    <|> gate right (not blockedR) `tag` pieceR
        case new of
            Event piece -> do
                arrM put -< piece
            NoEvent -> returnA -< ()
  where
    occupied p@(P (V2 x _)) = Any (x > 4 || x < (-5) || M.member p obstacles)

-- | The rotational motion of the current piece
pieceRotates ::
    M.Map (Point V2 Int) a
    -> Task (Sensation (Lightarrow ())) () (StateT PieceState (Lightarrow ())) c
pieceRotates obstacles = always $
    proc s -> do
        clockwise <- edge -< keyPressed (charKey 'x') s
        counterclockwise <- edge -< keyPressed (charKey 'z') s
        piece <- constM get -< ()
        let pieceClock = piece & _rotation %~ subtract (pi / 2)
            pieceCounter = piece & _rotation %~ (+ (pi / 2))
            blockedClock = getAny (foldMap occupied (points pieceClock))
            blockedCounter = getAny (foldMap occupied (points pieceCounter))
            command =
                gate clockwise (not blockedClock) `tag` pieceClock
                    <|> gate counterclockwise (not blockedCounter) `tag` pieceCounter
        case command of
            Event piece -> do
                arrM put -< piece
            NoEvent -> returnA -< ()
  where
    occupied p@(P (V2 x y)) = Any (x > 4 || x < (-5) || y < (-10) || M.member p obstacles)
