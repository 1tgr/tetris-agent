module Tetris.Evolution.Markov where

import Array
import Debug.Trace
import Random
import Tetris.Engine
import Tetris.Evolution

data MarkovState = MarkovState (PieceCode, PieceCode, PieceCode)
                   deriving (Bounded, Eq, Ix, Ord, Show)

data MarkovIndividual = MarkovIndividual
                        {
                            lastState :: MarkovState,
                            lastPosition :: Int,
                            offsets :: Array MarkovState Int,
                            rotations :: Array MarkovState Rotation
                        }
                        deriving (Show)

randomArray :: (Bounded a, Bounded b, Enum b, Ix a, RandomGen g) => (g -> (b, g)) -> g -> (g, Array a b)
randomArray generator g =
    (g', listArray indexRange items)
    where
        indexRange = (minBound, maxBound)
        (g', items) = randomItems g $ rangeSize indexRange

        randomItems g 0 =
            (g, [ ])

        randomItems g n =
            (g'', item : items)
            where
                (g', items) = randomItems g (n - 1)
                (item, g'') = generator g'

mutateArray :: (Bounded a, Bounded b, Enum b, Ix a, Num b, RandomGen g) => (g -> (b, g)) -> g -> Array a b -> (g, Array a b)
mutateArray generator g a =
    (g', a')
    where
        (g', changes) = randomArray generator g
        indexRange = (minBound, maxBound)
        a' = array indexRange $ map f $ assocs a
        f (a, b) = (a, b + changes!a)

pushState :: PieceCode -> MarkovState -> MarkovState
pushState s4 (MarkovState (_, s2, s3)) =
    MarkovState (s2, s3, s4)

instance Individual MarkovIndividual where
    player individual pieceCode board =
        (individual { lastState = state, lastPosition = position }, position, rotation)
        where
            state = pushState pieceCode $ lastState individual
            offset = (!state) $ offsets individual
            position = (abs $ offset + lastPosition individual) `mod` 10
            rotation = (!state) $ rotations individual

    randomIndividual g =
        (g'', MarkovIndividual { lastState = MarkovState (I, I, I), lastPosition = 5, offsets = offsets, rotations = rotations })
        where
            (g', offsets) = randomArray (randomR (-5, 5)) g
            (g'', rotations) = randomArray random g'

    mutateIndividual g individual @ (MarkovIndividual { offsets = offsets }) = 
        (g', individual { offsets = offsets' })
        where
            (g', offsets') = mutateArray (randomR (-5, 5)) g offsets
