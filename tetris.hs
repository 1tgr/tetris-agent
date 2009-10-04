{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables, OverlappingInstances #-}
import Array
import Debug.Trace
import HUnit
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

                        drawPiece [ ] [ ] =
                            [ ]

                bottom = drop (row + pieceLength - 1) board
                pieceLength = length piece

playGame :: RandomGen g => g -> (a -> PieceCode -> Board -> (a, Int, Rotation)) -> a -> (Int, Board)
playGame g player state =
    playGameInner g state (0, emptyBoard)
    where
        playGameInner g state (score, board) =
            let
                (pieceCode, g') = random g
                (state', position, rotation) = player state pieceCode board
                pieceData = trace (show (position, rotation)) $ rotatedPiece pieceCode rotation
            in
                case updateBoard pieceData board position of
                Just updatedBoard -> playGameInner g' state' $ collapse score updatedBoard
                Nothing -> (score, board)

depth :: Board -> [ Int ]
depth board =
    depthInner board 0 $ replicate (maximum $ map length board) 0
    where
        depthInner (x : xs) depth depths =
            depthInner xs (depth + 1) $ latchDepth x depths
            where
                latchDepth ('.' : bs) (_ : ds) = depth : latchDepth bs ds
                latchDepth (_ : bs) (d : ds)  = d : latchDepth bs ds
                latchDepth [ ] _ = [ depth ]
                latchDepth _ [ ] = [ depth ]

        depthInner [ ] _ depths = depths

randomPlayer :: RandomGen g => g -> PieceCode -> Board -> (g, Int, Rotation)
randomPlayer g pieceCode board =
    (g', position, rotation)
    where
        (rotation, g') = random g
        pieceData = rotatedPiece pieceCode rotation
        pieceWidth = max $ map length pieceData

        f (maxDepth, maxPosition, position) depth =
            if depth > maxDepth then
                (depth, position, position + 1)
            else
                (maxDepth, maxPosition, position + 1)

        (_, position, _) = foldl f (0, 0, 0) $ depth board

testRandomPlayer = do
    let g = mkStdGen 0
    let (score, board) = playGame g randomPlayer g
    mapM_ putStrLn board
    score @?= 0

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
            "randomPlayer should work" ~: testRandomPlayer
        ]
