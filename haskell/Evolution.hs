{-# LANGUAGE MultiParamTypeClasses #-}
module Tetris.Evolution where
  
import Control.Monad.State.Lazy
import Control.Parallel.Strategies
import Debug.Trace
import Random
import List
import Tetris.Engine

class Show i => Individual i where
    player :: PieceCode -> Board -> State i (Int, Rotation)
    randomIndividual :: RandomGen g => State g i
    mutateIndividual :: RandomGen g => i -> State g i

type Fitness i = (i, (Int, Int, Int))
type Population i = [ Fitness i ]

fitness :: Individual i => i -> Fitness i
fitness individual = (individual, (totalScore * totalTurns, totalScore, totalTurns))
    where (totalScore, totalTurns) = foldl (\(score, turns) (score', turns', _) -> (score + score', turns + turns')) (1, 1) 
                                   $ flip evalState (mkStdGen 0)
                                   $ sequence
                                   $ replicate 100 (playGame player individual)

randomPopulation :: (Individual i, RandomGen g) => Int -> State g (Population i)
randomPopulation count = sequence 
                       $ replicate count 
                       $ randomIndividual >>= return . fitness

mutatePopulation :: (Individual i, RandomGen g) => Int -> Population i -> State g (Population i)
mutatePopulation generation population = do
    individuals' <- sequence $ replicate (count - 1) $ mutateIndividual fittestIndividual
    let population' = parMap rwhnf fitness individuals'
    return (fittest : population')
    where fittest @ (fittestIndividual, fittestScore) = maximumBy (\ (_, a) (_, b) -> compare a b) population
          message = "Generation " ++ (show generation) ++ ": " ++ (show $ fittestScore)
          count = trace message $ length population

evolve :: (Individual i, RandomGen g) => State g (Fitness i)
evolve = do
    initialPopulation <- randomPopulation 100
    finalPopulation <- foldM (flip ($)) initialPopulation 
                    $ zipWith (flip ($)) [ 1 .. 1000 ] 
                    $ repeat mutatePopulation
    return $ maximumBy (\ (_, a) (_, b) -> compare a b) finalPopulation
