module Main where

import HUnit
import List
import Random
import Tetris.Engine
import Tetris.Evolution
import Tetris.Evolution.Markov
import Tetris.Evolution.Poly
import Tetris.Player.Random

main =
    runTestTT $
        TestList
        [
            "Given empty board, collapse should produce empty board" ~: collapse 0 emptyBoard ~?= (0, emptyBoard),
            "Given full board, collapse should produce empty board" ~: collapse 0 fullBoard ~?= (20, emptyBoard),
            "Given partial board, collapse should remove rull rows" ~: collapse 0 partialBoard ~?= (9, (replicate 19 emptyRow) ++ [ partialRow ]),
            "Given empty board, piece should drop to bottom" ~: dropPiece (piece O) emptyBoard 0 ~?= Just 19,
            "Given full board, piece should not drop" ~: dropPiece (piece O) fullBoard 0 ~?= Nothing,
            "Given partial board, central piece should drop halfway" ~: dropPiece (piece O) partialBoard 4 ~?= Just 10,
            "Given partial board, offset piece should drop halfway" ~: dropPiece (piece O) partialBoard 3 ~?= Just 9,
            "Given partial board, left piece should drop halfway" ~: dropPiece (piece O) partialBoard 0 ~?= Just 9,
            "Given full board, should not update" ~: updateBoard (piece O) fullBoard 4 ~?= Nothing,
            "Given empty board, should update with central piece" ~: updateBoard (piece O) emptyBoard 4 ~?= Just emptyBoardWithPiece,
            "Given partial board, should update with central piece" ~: updateBoard (piece O) partialBoard 4 ~?= Just partialBoardWithPiece,

            "randomPlayer should work" ~:
                let
                    g = mkStdGen 0
                    (_, score, turns, board) = playGame g randomPlayer g in
                score ~?= 0,

            "MarkovIndividual should work" ~: 
                let
                    state = randomPopulation (mkStdGen 0) 100

                    population :: Population MarkovIndividual
                    population = snd $ evolver state 1000

                    fittest = maximumBy (\ (_, a) (_, b) -> compare a b) population
                in do
                    putStrLn ""
                    putStrLn $ show fittest
                    return (),

            "PolyIndividual should work" ~: 
                let
                    state = randomPopulation (mkStdGen 0) 100

                    population :: Population PolyIndividual
                    population = snd $ evolver state 1000

                    fittest = maximumBy (\ (_, a) (_, b) -> compare a b) population
                in do
                    putStrLn ""
                    putStrLn $ show fittest
                    return ()
        ]
