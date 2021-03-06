module Tetris.Evolution.Markov where

import Array
import Control.Monad.State.Lazy
import Random
import Tetris.Engine
import Tetris.Evolution

data MarkovState = MarkovState (PieceCode, PieceCode, PieceCode, PieceCode)
                   deriving (Bounded, Eq, Ix, Ord, Show)

data MarkovIndividual = MarkovIndividual
                        {
                            lastState :: MarkovState,
                            lastPosition :: Int,
                            offsets :: Array MarkovState Int,
                            rotations :: Array MarkovState Rotation
                        }
                        deriving (Show)

randomArray :: (Bounded a, Ix a) => State g b -> State g (Array a b)
randomArray generator =
    (sequence $ replicate (rangeSize indexRange) generator) >>= return . listArray indexRange
    where indexRange = (minBound, maxBound)

mutateArray :: (Bounded a, Ix a, Num b) => (State g b) -> Array a b -> State g (Array a b)
mutateArray generator a = do
    changes <- randomArray generator
    return $ array (minBound, maxBound) 
           $ map (\(i, j) -> (i, j + changes!i))
           $ assocs a

pushState :: PieceCode -> MarkovState -> MarkovState
pushState s5 (MarkovState (_, s2, s3, s4)) =
    MarkovState (s2, s3, s4, s5)

instance Individual MarkovIndividual where
    player pieceCode _ = do
        individual <- get
        let state = pushState pieceCode $ lastState individual
            offset = (!state) $ offsets individual
            position = (abs $ offset + lastPosition individual) `mod` 10
            rotation = (!state) $ rotations individual
        put $ individual { lastState = state, lastPosition = position }
        return (position, rotation)

    randomIndividual = do
        offsets' <- randomArray State { runState = randomR (-5, 5) }
        rotations' <- randomArray State { runState = random }
        return $ MarkovIndividual { lastState = minBound, lastPosition = 5, offsets = offsets', rotations = rotations' }

    mutateIndividual individual @ (MarkovIndividual { offsets = offsets' }) =do
        offsets'' <- mutateArray State { runState = randomR (-5, 5) } offsets'
        return $ individual { offsets = offsets'' }
