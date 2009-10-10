module Tetris.Player.Random where

import Random
import Tetris.Engine

randomPlayer :: RandomGen g => g -> PieceCode -> Board -> (g, Int, Rotation)
randomPlayer g pieceCode board =
    (g', position, rotation)
    where
        (rotation, g') = random g
        pieceData = rotatedPiece pieceCode rotation
        pieceWidth = maximum $ map length pieceData
        boardWidth = maximum $ map length board

        f (maxDepth, maxPosition, position) depth =
            if depth > maxDepth then
                (depth, position, position + 1)
            else
                (maxDepth, maxPosition, position + 1)

        (_, position, _) = foldl f (0, 0, 0) $ take (boardWidth - pieceWidth + 1) $ depths board
