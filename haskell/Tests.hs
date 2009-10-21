module Main where

import Control.Monad.State.Lazy
import HUnit
import List
import Random
import Tetris.Engine
import Tetris.Evolution
import Tetris.Evolution.Markov
import Tetris.Evolution.Poly
import Tetris.Player.Random

emptyRowWithPiece :: String
emptyRowWithPiece = "....oo...."

emptyBoardWithPiece :: Board
emptyBoardWithPiece = (replicate 18 emptyRow) ++ [ emptyRowWithPiece, emptyRowWithPiece ]

fullRow :: String
fullRow = replicate 10 'I'

fullBoard :: Board
fullBoard = replicate 20 fullRow

partialRow :: String
partialRow = "ssss..ssss"

partialRowWithPiece :: String
partialRowWithPiece = "ssssoossss"

partialBoard :: Board
partialBoard = (replicate 10 emptyRow) ++ [ partialRow ] ++ (replicate 9 fullRow)

partialBoardWithPiece :: Board
partialBoardWithPiece = (replicate 9 emptyRow) ++ [ emptyRowWithPiece, partialRowWithPiece ] ++ (replicate 9 fullRow)

piece :: PieceCode -> Piece
piece = flip rotatedPiece $ None

main :: IO Counts
main =
    runTestTT $
        TestList
        [
            "Given empty board, collapse should produce empty board" ~: collapse emptyBoard 0 ~?= (emptyBoard, 0),
            "Given full board, collapse should produce empty board" ~: collapse fullBoard 0 ~?= (emptyBoard, 20),
            "Given partial board, collapse should remove rull rows" ~: collapse partialBoard 0 ~?= ((replicate 19 emptyRow) ++ [ partialRow ], 9),
            "Given empty board, piece should drop to bottom" ~: dropPiece (piece O) emptyBoard 0 ~?= Just 19,
            "Given full board, piece should not drop" ~: dropPiece (piece O) fullBoard 0 ~?= Nothing,
            "Given partial board, central piece should drop halfway" ~: dropPiece (piece O) partialBoard 4 ~?= Just 10,
            "Given partial board, offset piece should drop halfway" ~: dropPiece (piece O) partialBoard 3 ~?= Just 9,
            "Given partial board, left piece should drop halfway" ~: dropPiece (piece O) partialBoard 0 ~?= Just 9,
            "Given full board, should not update" ~: updateBoard (piece O) fullBoard 4 ~?= Nothing,
            "Given empty board, should update with central piece" ~: updateBoard (piece O) emptyBoard 4 ~?= Just emptyBoardWithPiece,
            "Given partial board, should update with central piece" ~: updateBoard (piece O) partialBoard 4 ~?= Just partialBoardWithPiece,

            "randomPlayer should work" ~:
                let g = mkStdGen 0
                    (score, _, _) = evalState (playGame randomPlayer g) g in
                score ~?= 0,

            "MarkovIndividual should work" ~: 
                let fittest :: Fitness MarkovIndividual
                    fittest = evalState evolve $ mkStdGen 0
                in do
                    putStrLn "\nRunning MarkovIndividual"
                    putStrLn $ show fittest
                    return (),

            "PolyIndividual should work" ~: 
                let fittest :: Fitness PolyIndividual
                    fittest = evalState evolve $ mkStdGen 0
                in do
                    putStrLn "\nRunning PolyIndividual"
                    putStrLn $ show fittest
                    return ()
        ]
