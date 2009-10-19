{-# LANGUAGE MultiParamTypeClasses #-}
module Tetris.Evolution where
  
import Control.Monad.State.Lazy
import Debug.Trace
import Random
import List
import Tetris.Engine

class Show i => Individual i where
    player :: PieceCode -> Board -> State i (Int, Rotation)
    randomIndividual :: RandomGen g => State g i
    mutateIndividual :: RandomGen g => i -> State g i

type Population i = [ (i, (Int, Int, Int)) ]

fitness :: Individual i => i -> (Int, Int, Int)
fitness individual =
    (totalScore * totalTurns, totalScore, totalTurns)
    where
        playGames 0 =
            return (1, 1)

        playGames count = do
            (score', turns', _) <- playGame player individual
            (score, turns) <- playGames (count - 1)
            return (score + score', turns + turns')

        (totalScore, totalTurns) = evalState (playGames 100) (mkStdGen 0) 

randomPopulation :: (Individual i, RandomGen g) => Int -> State g (Population i)
randomPopulation = sequence . (flip replicate $ randomIndividual >>= \individual -> return (individual, fitness individual))

mutatePopulation :: (Individual i, RandomGen g) => Int -> Population i -> State g (Population i)
mutatePopulation generation population =
    mutatePopulationInner (count - 1) $ trace ("Generation " ++ (show generation) ++ ": " ++ (show $ snd fittest)) $ [ fittest ]
    where
        fittest = maximumBy (\ (_, a) (_, b) -> compare a b) population
        count = length population

        mutatePopulationInner 0 population = 
            return population

        mutatePopulationInner count population = do
            individual <- mutateIndividual $ fst fittest
            population' <- mutatePopulationInner (count - 1) population
            return $ (individual, fitness individual) : population'

evolver :: (Individual i, RandomGen g) => Int -> Population i -> State g (Population i)
evolver 0 population = return population
evolver iterations population = mutatePopulation (1000 - iterations) population >>= evolver (iterations - 1)
