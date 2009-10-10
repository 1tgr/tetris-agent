module Tetris.Evolution.Poly where
  
import Random
import Tetris.Engine
import Tetris.Evolution

data PolyIndividual = PolyIndividual ([ (Double, Double, Double) ], [ (Double, Double, Double) ])
                    deriving (Show)

randomFactor :: RandomGen g => g -> (g, (Double, Double, Double))
randomFactor g =
    (g''', (a - 0.5, b - 0.5, c - 0.5))
    where
        (a, g') = random g
        (b, g'') = random g'
        (c, g''') = random g''

mutateFactor :: RandomGen g => g -> (Double, Double, Double) -> (g, (Double, Double, Double))
mutateFactor g (a, b, c) =
    (g', (a + a', b + b', c + c'))
    where
        (g', (a', b', c')) = randomFactor g

instance Individual PolyIndividual where
    player (PolyIndividual (positionFactors, rotationFactors)) state pieceCode board =
        (state, position, rotation)
        where
            d = map fromIntegral $ depths board

            applyFactors :: Double -> (Double, Double, Double) -> Double
            applyFactors depth (a, b, c) = 
                a * depth ^^ 2 + b * depth + c

            position = (truncate $ sum $ zipWith applyFactors d positionFactors) `mod` 10
            rotation = toEnum $ (truncate $ sum $ zipWith applyFactors d rotationFactors) `mod` 4

    randomIndividual g =
        randomIndividual' g 10
        where
            randomIndividual' g 0 =
                (g, PolyIndividual ([ ], [ ]))

            randomIndividual' g count =
                (g''', PolyIndividual (positionFactor : positionFactors, rotationFactor : rotationFactors))
                where
                    (g', positionFactor) = randomFactor g
                    (g'', rotationFactor) = randomFactor g'
                    (g''', PolyIndividual (positionFactors, rotationFactors)) = randomIndividual' g'' (count - 1)

    mutateIndividual g (PolyIndividual (positions, rotations)) = 
        (g'', PolyIndividual (positions', rotations'))
        where
            (g', positions') = mutateList g positions
            (g'', rotations') = mutateList g' rotations
            
            mutateList g [ ] =
                (g, [ ])

            mutateList g (x : xs) =
                (g'', x' : xs')
                where
                    (g', x') = mutateFactor g x
                    (g'', xs') = mutateList g' xs
