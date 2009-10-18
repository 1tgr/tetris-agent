module Tetris.Evolution.Markov where

import Array
import Control.Monad.State.Lazy
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

randomList :: (g -> (b, g)) -> Int -> State g [ b ]
randomList _ 0 =
    return [ ]

randomList generator n = do
    g <- get
    let (item, g') = generator g
    put g'
    items <- randomList generator (n - 1)
    return $ item : items

randomArray :: (Bounded a, Bounded b, Enum b, Ix a, RandomGen g) => (g -> (b, g)) -> State g (Array a b)
randomArray generator = do
    items <- randomList generator $ rangeSize indexRange
    return $ listArray indexRange items
    where indexRange = (minBound, maxBound)

mutateArray :: (Bounded a, Bounded b, Enum b, Ix a, Num b, RandomGen g) => (g -> (b, g)) -> Array a b -> State g (Array a b)
mutateArray generator a = do
    changes <- randomArray generator
    let f (a, b) = (a, b + changes!a)
    return $ array (minBound, maxBound) $ map f $ assocs a

pushState :: PieceCode -> MarkovState -> MarkovState
pushState s4 (MarkovState (_, s2, s3)) =
    MarkovState (s2, s3, s4)

instance Individual MarkovIndividual where
    player individual pieceCode board =
        (individual { lastState = state, lastPosition = position }, position, rotation)
        where state = pushState pieceCode $ lastState individual
              offset = (!state) $ offsets individual
              position = (abs $ offset + lastPosition individual) `mod` 10
              rotation = (!state) $ rotations individual

    randomIndividual g =
        (MarkovIndividual { lastState = MarkovState (I, I, I), lastPosition = 5, offsets = offsets, rotations = rotations }, g')
        where zob = do
                    offsets <- randomArray (randomR (-5, 5))
                    rotations <- randomArray random
                    return (offsets, rotations)
              ((offsets, rotations), g') = runState zob g

    mutateIndividual individual @ (MarkovIndividual { offsets = offsets }) g = 
        (individual { offsets = offsets' }, g')
        where (offsets', g') = runState (mutateArray (randomR (-5, 5)) offsets) g
