module Tetris.Grid where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Lightarrow hiding (rotation, _rotation)
import Linear (V2 (V2), V3 (V3))
import Linear.Affine (Point (P))
import System.Lightarrow.Run (Lightarrow)

-- | Remove full rows from grid, and slide the rows above down to the next
-- incomplete row
clearFullRows :: M.Map (Point V2 Int) a -> M.Map (Point V2 Int) a
clearFullRows grid = foldl' clearRow grid [-10 .. 9]
  where
    clearRow grid0 y
        | (True, grid1) <- foldl'
                            (\(full, grid) x ->
                                let (cleared, grid') =
                                        M.updateLookupWithKey
                                            (const (const Nothing))
                                            (P (V2 x y))
                                            grid
                                 in (full && isJust cleared, grid'))
                            (True, grid0)
                            [-5 .. 4]
            = clearRow (shiftDown grid1 y) y
        | otherwise
            = grid0
    shiftDown grid0 y0
        = M.foldlWithKey'
            ( \grid p@(P (V2 x y)) c ->
                if y > y0
                    then M.insert (P (V2 x (y - 1))) c (M.delete p grid)
                    else grid
            )
            grid0
            grid0

-- | Draw a set of grid tiles
drawTiles :: M.Map (Point V2 Int) Color -> Actuation (Lightarrow ())
drawTiles
    = (<> drawBoard) .
        M.foldlWithKey'
            ( \output (P (V2 x y)) color ->
                output
                    <> rectangle
                        color
                        (V2 50 50)
                        (fmap fromIntegral (V3 (50 * x) (50 * y) 1))
            )
            mempty

-- | Draw the empty board background
drawBoard :: Actuation (Lightarrow ())
drawBoard = rectangle Black (V2 w h) (V3 (- w / 2) (- h / 2) 0)
  where
    w = 500 -- 50 * 10
    h = 1000