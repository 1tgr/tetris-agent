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

type Population i = [ (i, (Int, Int, Int)) ]

splitM :: RandomGen g => State g g
splitM = do
  g <- get
  let (g', g'') = split g
  put g'
  return g''

fitness :: Individual i => i -> (Int, Int, Int)
fitness individual =
    (totalScore * totalTurns, totalScore, totalTurns)
    where (totalScore, totalTurns) = foldl (\(score, turns) (score', turns', _) -> (score + score', turns + turns')) (1, 1) 
                                   $ parMap rwhnf (evalState $ playGame player individual) 
                                   $ evalState (sequence $ replicate 100 splitM) 
                                   $ mkStdGen 0

randomPopulation :: (Individual i, RandomGen g) => Int -> State g (Population i)
randomPopulation = sequence . (flip replicate $ randomIndividual >>= \individual -> return (individual, fitness individual))

mutatePopulation :: (Individual i, RandomGen g) => Int -> Population i -> State g (Population i)
mutatePopulation generation population =
    sequence $ keepFittest : mutateOthers
    where fittest = maximumBy (\ (_, a) (_, b) -> compare a b) population
          count = trace ("Generation " ++ (show generation) ++ ": " ++ (show $ snd fittest)) $ length population
          keepFittest = return fittest
          mutateOthers = replicate (count - 1) $ do
              individual <- mutateIndividual $ fst fittest
              return $ (individual, fitness individual)

evolver :: (Individual i, RandomGen g) => Int -> Population i -> State g (Population i)
evolver 0 population = return population
evolver iterations population = do
    population' <- mutatePopulation (1000 - iterations) population
    evolver (iterations - 1) population'
