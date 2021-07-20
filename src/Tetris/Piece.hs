module Tetris.Piece where

import Lightarrow hiding (rotation, _rotation)
import Linear (V2 (..), V4 (V4), (!*))
import Linear.Affine (Point (..))
import Optics (lens, Lens)

-- | State variables of the current piece
data PieceState = PieceState
    { location :: Point V2 Double
    , rotation :: Double
    , shape :: PieceShape
    , speed :: Double
    }

-- | Which of the seven shapes a piece has
data PieceShape
    = Ell
    | Ess
    | Jay
    | Square
    | Straight
    | Tee
    | Zee
    deriving (Bounded, Enum)

_location :: Lens PieceState PieceState (Point V2 Double) (Point V2 Double)
_location = lens location (\(PieceState _ r s v) l -> PieceState l r s v)

_rotation :: Lens PieceState PieceState Double Double
_rotation = lens rotation (\(PieceState l _ s v) r -> PieceState l r s v)

_shape :: Lens PieceState PieceState PieceShape PieceShape
_shape = lens shape (\(PieceState l r _ v) s -> PieceState l r s v)

_speed :: Lens PieceState PieceState Double Double
_speed = lens speed (\(PieceState l r s _) v -> PieceState l r s v)

-- | The tiles of a piece at a certain location and rotation
points :: PieceState -> [Point V2 Int]
points (PieceState x r shape _) = fmap (fmap round . transformRigid2 x r) tiles
  where
    tiles = case shape of
        Ell         -> [P (V2 (-1) 0),  P (V2 0 0),     P (V2 1 0),     P (V2 1 1)]
        Ess         -> [P (V2 (-1) 0),  P (V2 0 0),     P (V2 0 1),     P (V2 1 1)]
        Jay         -> [P (V2 (-1) 1),  P (V2 (-1) 0),  P (V2 0 0),     P (V2 1 0)]
        Square      -> [P (V2 0 0),     P (V2 1 0),     P (V2 0 1),     P (V2 1 1)]
        Tee         -> [P (V2 (-1) 0),  P (V2 0 0),     P (V2 1 0),     P (V2 0 1)]
        Straight    -> [P (V2 (-1) 0),  P (V2 0 0),     P (V2 1 0),     P (V2 2 0)]
        Zee         -> [P (V2 (-1) 1),  P (V2 0 1),     P (V2 0 0),     P (V2 1 0)]

-- | Rotate a 2D point about the origin, and translate it by the vector from
-- the origin to a given point.
transformRigid2 :: Point V2 Double -> Double -> Point V2 Double -> Point V2 Double
transformRigid2 x r = trim . (toMatrix (rigid2 x r) !*) . extend
  where
    trim (V4 x y _ _) = P (V2 x y)
    extend (P (V2 x y)) = V4 x y 0 1