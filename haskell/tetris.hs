{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables, OverlappingInstances #-}
import Array
import Debug.Trace
import HUnit
import List
import Random

type Board = [ String ]
type Piece = [ String ]

data PieceCode = I
               | J
               | L
               | O
               | S
               | T
               | Z
               deriving (Bounded, Enum, Show)

data Rotation = None
              | Ninety
              | OneEighty
              | TwoSeventy
              deriving (Bounded, Enum, Show)

class (Bounded a, Enum a) => BoundedEnum a
instance (Bounded a, Enum a) => BoundedEnum a

instance BoundedEnum a => Random a where
    random g =
        let min = fromEnum (minBound :: a) in
        let max = fromEnum (maxBound :: a) in
        let (i, g') = randomR (min, max) $ g in
        (toEnum i, g')
    randomR (low, high) g =
        let min = fromEnum low in
        let max = fromEnum high in
        let (i, g') = randomR (min, max) $ g in
        (toEnum i, g')

emptyRow = replicate 10 '.'
emptyRowWithPiece = "....oo...."
emptyBoard = replicate 20 emptyRow
emptyBoardWithPiece = (replicate 18 emptyRow) ++ [ emptyRowWithPiece, emptyRowWithPiece ]
fullRow = replicate 10 'I'
fullBoard = replicate 20 fullRow
partialRow = "ssss..ssss"
partialRowWithPiece = "ssssoossss"
partialBoard = (replicate 10 emptyRow) ++ [ partialRow ] ++ (replicate 9 fullRow)
partialBoardWithPiece = (replicate 9 emptyRow) ++ [ emptyRowWithPiece, partialRowWithPiece ] ++ (replicate 9 fullRow)

rotatedPiece :: PieceCode -> Rotation -> Piece
rotatedPiece I None = [ "i",
                        "i",
                        "i",
                        "i" ]
                        
rotatedPiece I Ninety = [ "iiii" ]

rotatedPiece I OneEighty = rotatedPiece I Ninety
rotatedPiece I TwoSeventy = rotatedPiece I None

rotatedPiece J None = [ ".j",
                        ".j",
                        "jj" ]

rotatedPiece J Ninety = [ "j..",
                          "jjj" ]

rotatedPiece J OneEighty = [ "jj",
                             "j.",
                             "j." ]

rotatedPiece J TwoSeventy = [ "jjj",
                              "..j" ]

rotatedPiece L None = [ "l.",
                        "l.",
                        "ll" ]
                        
rotatedPiece L Ninety = [ "lll",
                          "l.." ]

rotatedPiece L OneEighty = [ "ll",
                             ".l",
                             ".l" ]

rotatedPiece L TwoSeventy = [ "..l",
                             "lll" ]

rotatedPiece O _ = [ "oo", 
                     "oo"]
      
rotatedPiece S None = [ ".ss",
                        "ss." ]
                        
rotatedPiece S Ninety = [ "s.",
                          "ss",
                          ".s" ]

rotatedPiece S OneEighty = rotatedPiece S None
rotatedPiece S TwoSeventy = rotatedPiece S Ninety

rotatedPiece T None = [ "ttt",
                        ".t." ]

rotatedPiece T Ninety = [ ".t",
                          "tt",
                          ".t" ]

rotatedPiece T OneEighty = [ ".t.",
                             "ttt" ]

rotatedPiece T TwoSeventy = [ "t.",
                              "tt",
                              "t." ]

rotatedPiece Z None = [ "zz.",
                        ".zz" ]

rotatedPiece Z Ninety = [ ".z",
                          "zz",
                          "z." ]

rotatedPiece Z OneEighty = rotatedPiece Z None
rotatedPiece Z TwoSeventy = rotatedPiece Z Ninety

piece :: PieceCode -> Piece
piece pieceCode = rotatedPiece pieceCode None

collapse :: Int -> Board -> (Int, Board)
collapse score board =
    collapseInner [ ] score board
    where
        collapseInner prior score (x : xs) =
            if any (== '.') x then
                collapseInner (x : prior) score xs
            else
                collapseInner (prior ++ [ emptyRow ]) (score + 1) xs

        collapseInner prior score [ ] = 
            (score, reverse prior)

dropPiece :: Piece -> Board -> Int -> Maybe Int
dropPiece piece board position =
    case dropPieceInner (-2) $ map (drop position) $ emptyRow : emptyRow : board of
    row | row >= 0 -> Just row
    _ -> Nothing
    where
        dropPieceInner row boardInner @ (_ : bs) =
            if rowsCollided piece boardInner then
                row
            else
                dropPieceInner (row + 1) bs
            where
                rowsCollided (p : ps) (b : bs) =
                    cellsCollided p b || rowsCollided ps bs
                    where
                        cellsCollided (_ : pps) ('.' : bbs) = cellsCollided pps bbs
                        cellsCollided ('.' : pps) (_ : bbs) = cellsCollided pps bbs
                        cellsCollided [ ] _ = False
                        cellsCollided _ _ = True

                rowsCollided [ ] _ = False
                rowsCollided _ [ ] = False

        dropPieceInner row [ ] = row - length piece + 1

updateBoard :: Piece -> Board -> Int -> Maybe Board
updateBoard piece board position = do
    row <- dropPiece piece board position
    return $ updateBoardInner row
    where
        updateBoardInner row =
            top ++ middle ++ bottom
            where
                top = take (row - 1) board
                middle =
                    drawPiece piece $ take pieceLength $ drop (row - 1) board
                    where
                        drawPiece (p : ps) (b : bs) =
                            [ left ++ centre ++ right ] ++ drawPiece ps bs
                            where
                                pieceWidth = length p
                                left = take position b
                                centre = zipWith drawCell p $ take pieceWidth $ drop position b
                                    where
                                        drawCell '.' bb = bb
                                        drawCell pp _ = pp

                                right = drop (position + pieceWidth) b

                        drawPiece [ ] _ = [ ]
                        drawPiece _ [ ] = [ ]

                bottom = drop (row + pieceLength - 1) board
                pieceLength = length piece

playGame :: RandomGen g => g -> (a -> PieceCode -> Board -> (a, Int, Rotation)) -> a -> (g, Int, Int, Board)
playGame g player state =
    playGameInner g state (0, 0, emptyBoard)
    where
        playGameInner g state (score, turns, board) =
            let
                (pieceCode, g') = random g
                (state', position, rotation) = player state pieceCode board
                pieceData = rotatedPiece pieceCode rotation
            in
                case updateBoard pieceData board position of
                Just board' -> 
                    let (score', board'') = collapse score board' in
                    playGameInner g' state' (score', turns + 1, board'')
                Nothing -> (g', score, turns, board)

depths :: Board -> [ Int ]
depths =
    depthsInner 0
    where
        depthsInner row (x : xs) =
            case xs of
            [ ] -> rowDepths
            _ -> zipWith min rowDepths $ depthsInner (row + 1) xs
            where
                rowDepths = map (\c -> if c == '.' then 20 else row) x

randomPlayer :: RandomGen g => g -> PieceCode -> Board -> (g, Int, Rotation)
randomPlayer g pieceCode board =
    (g', position, rotation)
    where
        (rotation, g') = random g
        pieceData = rotatedPiece pieceCode rotation
        pieceWidth = maximum $ map length pieceData
        boardWidth = maximum $ map length board

        f (maxDepth, maxPosition, position) depth =
            if depth > maxDepth then
                (depth, position, position + 1)
            else
                (maxDepth, maxPosition, position + 1)

        (_, position, _) = foldl f (0, 0, 0) $ take (boardWidth - pieceWidth + 1) $ depths board


type Individual = ([ (Double, Double, Double) ], [ (Double, Double, Double) ])
type Population f = [ (Individual, f) ]

evolvingPlayer :: Individual -> () -> PieceCode -> Board -> ((), Int, Rotation)
evolvingPlayer (positionFactors, rotationFactors) _ pieceCode board =
    ((), position, rotation)
    where
        d = map fromIntegral $ depths board

        applyFactors :: Double -> (Double, Double, Double) -> Double
        applyFactors depth (a, b, c) = 
            a * depth ^^ 2 + b * depth + c

        position = (truncate $ sum $ zipWith applyFactors d positionFactors) `mod` 10
        rotation = toEnum $ (truncate $ sum $ zipWith applyFactors d rotationFactors) `mod` 4

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

randomIndividual :: RandomGen g => g -> Int -> (g, Individual)
randomIndividual g 0 =
    (g, ([ ], [ ]))

randomIndividual g count =
    (g''', (positionFactor : positionFactors, rotationFactor : rotationFactors))
    where
        (g', positionFactor) = randomFactor g
        (g'', rotationFactor) = randomFactor g'
        (g''', (positionFactors, rotationFactors)) = randomIndividual g'' (count - 1)

fitness individual =
    (totalScore * totalTurns, totalScore, totalTurns)
    where
        player = evolvingPlayer individual

        playGames g 0 =
            (g, 1, 1)

        playGames g count =
            (g'', score + score', turns + turns')
            where
                (g', score', turns', _) = playGame g (evolvingPlayer individual) ()
                (g'', score, turns) = playGames g' (count - 1)

        (_, totalScore, totalTurns) = playGames (mkStdGen 0) 100

-- randomPopulation :: (RandomGen g) => g -> Int -> (g, Population f)
randomPopulation g 0 =
    (g, [ ])

randomPopulation g count =
    (g'', (individual, fitness individual) : individuals)
    where
        (g', individual) = randomIndividual g 10
        (g'', individuals) = randomPopulation g' (count - 1)

mutateList :: RandomGen g => g -> [ (Double, Double, Double) ] -> (g, [ (Double, Double, Double) ])
mutateList g [ ] =
    (g, [ ])

mutateList g (x : xs) =
    (g'', x' : xs')
    where
        (g', x') = mutateFactor g x
        (g'', xs') = mutateList g' xs

mutateIndividual :: RandomGen g => g -> Individual -> (g, Individual)
mutateIndividual g (positions, rotations) = 
    (g'', (positions', rotations'))
    where
        (g', positions') = mutateList g positions
        (g'', rotations') = mutateList g' rotations

-- mutatePopulation :: (Num f, Ord f, Show f, RandomGen g) => (g, Population f) -> Int -> (g, Population f)
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

-- evolver :: (Num f, Ord f, RandomGen g) => (g, Population f) -> Int -> (g, Population f)
evolver state 0 =
    state

evolver state iterations =
    evolver (mutatePopulation state (1000 - iterations)) (iterations - 1)

main =
    runTestTT $
        TestList
        [
            "Given empty board, collapse should produce empty board" ~: collapse 0 emptyBoard ~?= (0, emptyBoard),
            "Given full board, collapse should produce empty board" ~: collapse 0 fullBoard ~?= (20, emptyBoard),
            "Given partial board, collapse should remove rull rows" ~: collapse 0 partialBoard ~?= (9, (replicate 19 emptyRow) ++ [ partialRow ]),
            "Given empty board, piece should drop to bottom" ~: dropPiece (piece O) emptyBoard 0 ~?= Just 19,
            "Given full board, piece should not drop" ~: dropPiece (piece O) fullBoard 0 ~?= Nothing,
            "Given partial board, central piece should drop halfway" ~: dropPiece (piece O) partialBoard 4 ~?= Just 10,
            "Given partial board, offset piece should drop halfway" ~: dropPiece (piece O) partialBoard 3 ~?= Just 9,
            "Given partial board, left piece should drop halfway" ~: dropPiece (piece O) partialBoard 0 ~?= Just 9,
            "Given full board, should not update" ~: updateBoard (piece O) fullBoard 4 ~?= Nothing,
            "Given empty board, should update with central piece" ~: updateBoard (piece O) emptyBoard 4 ~?= Just emptyBoardWithPiece,
            "Given partial board, should update with central piece" ~: updateBoard (piece O) partialBoard 4 ~?= Just partialBoardWithPiece,
            "randomPlayer should work" ~: do
                let g = mkStdGen 0
                let (_, score, turns, board) = playGame g randomPlayer g
                mapM_ putStrLn ("" : board)
                score @?= 0,
            "evolver should work" ~: do
                let state = randomPopulation (mkStdGen 0) 100
                let (_, population) = evolver state 1000
                let fittest = maximumBy (\ (_, a) (_, b) -> compare a b) population
                putStrLn ""
                putStrLn $ show fittest
                return ()
        ]
