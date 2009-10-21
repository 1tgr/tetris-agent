module Tetris.Evolution.Poly where

import Control.Monad.State.Lazy  
import Random
import Tetris.Engine
import Tetris.Evolution

data PolyIndividual = PolyIndividual ([ (Double, Double, Double) ], [ (Double, Double, Double) ])
                      deriving (Show)

replicate3 :: a -> (a, a, a)
replicate3 a = (a, a, a)

sequence3 :: Monad m => (m a, m a, m a) -> m (a, a, a)
sequence3 (ma, mb, mc) = ma >>= \a -> mb >>= \b -> mc >>= \c -> return (a, b, c)

mutateFactor :: Num a => (State g (a, a, a)) -> (a, a, a) -> State g (a, a, a)
mutateFactor generator (a, b, c) = generator >>= \(a', b', c') -> return (a + a', b + b', c + c')

instance Individual PolyIndividual where
    player _ board = do
        PolyIndividual (positionFactors, rotationFactors) <- get
        let position = (truncate $ sum $ zipWith applyFactors d positionFactors) `mod` 10
            rotation = toEnum $ (truncate $ sum $ zipWith applyFactors d rotationFactors) `mod` 4
        return (position, rotation)
        where applyFactors :: Double -> (Double, Double, Double) -> Double
              applyFactors depth (a, b, c) = a * depth ^^ (2 :: Integer) + b * depth + c
              d = map fromIntegral $ depths board

    randomIndividual = do
        positions' <- m
        rotations' <- m
        return $ PolyIndividual (positions', rotations')
        where m = sequence 
                $ replicate 10 
                $ sequence3 . replicate3 
                $ randomM 
                $ randomR (-0.5, 0.5)

    mutateIndividual (PolyIndividual (positions, rotations)) = do
        positions' <- m positions
        rotations' <- m rotations
        return $ PolyIndividual (positions', rotations')
        where m = mapM 
                $ mutateFactor 
                $ sequence3 . replicate3 
                $ randomM 
                $ randomR (-0.5, 0.5)
