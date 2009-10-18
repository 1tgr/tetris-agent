module Tetris.Evolution.Poly where

import Control.Monad.State.Lazy  
import Random
import Tetris.Engine
import Tetris.Evolution

data PolyIndividual = PolyIndividual ([ (Double, Double, Double) ], [ (Double, Double, Double) ])
                      deriving (Show)

randomM :: (g -> (a, g)) -> State g a
randomM generator = do
  g <- get
  let (n, g') = generator g
  put g'
  return n

randomFactor :: Fractional a => (State g a) -> State g (a, a, a)
randomFactor generator = do
    a <- generator
    b <- generator
    c <- generator
    return (a, b, c)

mutateFactor :: Num a => (State g (a, a, a)) -> (a, a, a) -> State g (a, a, a)
mutateFactor generator (a, b, c) = do
    (a', b', c') <- generator
    return (a + a', b + b', c + c')

mutateList :: (a -> State g a) -> [ a ] -> State g [ a ]
mutateList _ [ ] =
    return [ ]

mutateList mutator (x : xs) = do
    x' <- mutator x
    xs' <- mutateList mutator xs
    return $ x' : xs'

instance Individual PolyIndividual where
    player individual @ (PolyIndividual (positionFactors, rotationFactors)) pieceCode board =
        (individual, position, rotation)
        where
            d = map fromIntegral $ depths board

            applyFactors :: Double -> (Double, Double, Double) -> Double
            applyFactors depth (a, b, c) = 
                a * depth ^^ 2 + b * depth + c

            position = (truncate $ sum $ zipWith applyFactors d positionFactors) `mod` 10
            rotation = toEnum $ (truncate $ sum $ zipWith applyFactors d rotationFactors) `mod` 4

    randomIndividual = 
      runState zob
      where r = randomFactor $ randomM $ randomR (-0.5, 0.5)
            zob = randomIndividual' 10
            randomIndividual' 0 = return $ PolyIndividual ([ ], [ ])

            randomIndividual' count = do
                                      positionFactor <- r
                                      rotationFactor <- r
                                      PolyIndividual (positionFactors, rotationFactors) <- randomIndividual' $ count - 1
                                      return $ PolyIndividual (positionFactor : positionFactors, rotationFactor : rotationFactors)

    mutateIndividual (PolyIndividual (positions, rotations)) = 
        runState zob
        where m = mutateList $ mutateFactor $ randomFactor $ randomM $ randomR (-0.5, 0.5)
              zob = do
                    positions' <- m positions
                    rotations' <- m rotations
                    return $ PolyIndividual (positions', rotations')
