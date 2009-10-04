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

piece :: PieceCode -> Piece
piece I = [ "i",
            "i" ]

piece J = [ ".j",
            ".j",
            ".j",
            "jj" ]

piece L = [ "l.",
            "l.",
            "l.",
            "ll" ]

piece O = [ "oo", 
            "oo"]
      
piece S = [ "ss.",
            ".ss" ]

piece T = [ "ttt",
            ".t." ]

piece Z = [ ".zz",
      "zz." ]

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
            beginning ++ middle ++ end
            where
                beginning = take (row - 1) board
                middle =
                    drawPiece piece $ take pieceLength $ drop (row - 1) board
                    where
                        drawPiece (centre : ps) (b : bs) =
                            [ left ++ centre ++ right ] ++ drawPiece ps bs
                            where
                                left = take position b
                                right = drop (position + pieceWidth) b
                                pieceWidth = length centre

                        drawPiece [ ] [ ] =
                            [ ]

                end = drop (row + pieceLength - 1) board
                pieceLength = length piece

playGame :: RandomGen g => g -> (a -> PieceCode -> Board -> (a, Int, Int)) -> a -> (Int, Board)
playGame random player state =
    playGameInner random 0 state emptyBoard
    where
        playGameInner randomInner score stateInner board =
            case maybeUpdatedBoard of
            Just updatedBoard -> 
                let (newScore, collapsedBoard) = collapse score updatedBoard in
                playGameInner newRandom newScore newState collapsedBoard
            Nothing ->
                (score, board)
            where
                pieceCodeChoices = [ I, J, L, O, S, T, Z ]
                (pieceCodeIndex, newRandom) = randomR (0, length pieceCodeChoices - 1) randomInner
                pieceCode = pieceCodeChoices !! pieceCodeIndex
                (newState, position, rotation) = player stateInner pieceCode board
                pieceData = piece pieceCode
                maybeUpdatedBoard = updateBoard pieceData board position

randomPlayer :: RandomGen g => g -> PieceCode -> Board -> (g, Int, Int)
randomPlayer random pieceCode board =
    (random3, position, rotation)
    where
        (position, random2) = randomR (0, 9) random
        (rotation, random3) = randomR (0, 3) random

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
            "randomPlayer works" ~: (fst $ playGame (mkStdGen 0) randomPlayer (mkStdGen 0)) ~?= 0
        ]
