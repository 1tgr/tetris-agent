{-# LANGUAGE MultiParamTypeClasses #-}
module Tetris.Evolution where
  
import Debug.Trace
import Random
import List
import Tetris.Engine

class Show i => Individual i where
    player :: i -> PieceCode -> Board -> (i, Int, Rotation)
    randomIndividual :: RandomGen g => g -> (g, i)
    mutateIndividual :: RandomGen g => g -> i -> (g, i)

type Population i = [ (i, (Int, Int, Int)) ]

fitness :: Individual i => i -> (Int, Int, Int)
fitness individual =
    (totalScore * totalTurns, totalScore, totalTurns)
    where
        playGames g 0 =
            (g, 1, 1)

        playGames g count =
            (g'', score + score', turns + turns')
            where
                (g', score', turns', _) = playGame g player individual
                (g'', score, turns) = playGames g' (count - 1)

        (_, totalScore, totalTurns) = playGames (mkStdGen 0) 100

randomPopulation :: (Individual i, RandomGen g) => g -> Int -> (g, Population i)
randomPopulation g 0 =
    (g, [ ])

randomPopulation g count =
    (g'', (individual, fitness individual) : individuals)
    where
        (g', individual) = randomIndividual g
        (g'', individuals) = randomPopulation g' (count - 1)

mutatePopulation :: (Individual i, RandomGen g) => (g, Population i) -> Int -> (g, Population i)
mutatePopulation (g, population) generation =
    mutatePopulationInner (g, [ fittest ]) $ trace ("Generation " ++ (show generation) ++ ": " ++ (show $ snd fittest)) $ (count - 1)
    where
        fittest = maximumBy (\ (_, a) (_, b) -> compare a b) population
        count = length population

        mutatePopulationInner state 0 =
            state

        mutatePopulationInner (g, population) count =
            (g'', (individual, fitness individual) : population')
            where
                (g', individual) = mutateIndividual g $ fst fittest
                (g'', population') = mutatePopulationInner (g', population) (count - 1)

evolver :: (Individual i, RandomGen g) => (g, Population i) -> Int -> (g, Population i)
evolver state 0 =
    state

evolver state iterations =
    evolver (mutatePopulation state (1000 - iterations)) (iterations - 1)
