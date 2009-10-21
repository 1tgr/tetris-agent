module Tetris.Player.Random where

import Control.Monad.State.Lazy
import Random
import Tetris.Engine

randomPlayer :: RandomGen g => PieceCode -> Board -> State g (Int, Rotation)
randomPlayer pieceCode board = do
    rotation <- State { runState = random }
    let pieceData = rotatedPiece pieceCode rotation
        pieceWidth = maximum $ map length pieceData
        (_, position, _) = foldl f (0, 0, 0) $ take (boardWidth - pieceWidth + 1) $ depths board
    return (position, rotation)
    where
        boardWidth = maximum $ map length board

        f (maxDepth, maxPosition, position) depth =
            if depth > maxDepth then
                (depth, position, position + 1)
            else
                (maxDepth, maxPosition, position + 1)
