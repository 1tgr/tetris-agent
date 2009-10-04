import HUnit

type Board = [ String ]
type Piece = [ String ]

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
o = [ "oo", "oo" ]

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

tests = 
    TestList
    [
        TestLabel "Given empty board, collapse should produce empty board" $ collapse 0 emptyBoard ~?= (0, emptyBoard),
        TestLabel "Given full board, collapse should produce empty board" $ collapse 0 fullBoard ~?= (20, emptyBoard),
        TestLabel "Given partial board, collapse should remove rull rows" $ collapse 0 partialBoard ~?= (9, (replicate 19 emptyRow) ++ [ partialRow ]),
        TestLabel "Given empty board, piece should drop to bottom" $ dropPiece o emptyBoard 0 ~?= Just 19,
        TestLabel "Given full board, piece should not drop" $ dropPiece o fullBoard 0 ~?= Nothing,
        TestLabel "Given partial board, central piece should drop halfway" $ dropPiece o partialBoard 4 ~?= Just 10,
        TestLabel "Given partial board, offset piece should drop halfway" $ dropPiece o partialBoard 3 ~?= Just 9,
        TestLabel "Given partial board, left piece should drop halfway" $ dropPiece o partialBoard 0 ~?= Just 9,
        TestLabel "Given full board, should not update" $ updateBoard o fullBoard 4 ~?= Nothing,
        TestLabel "Given empty board, should update with central piece" $ updateBoard o emptyBoard 4 ~?= Just emptyBoardWithPiece,
        TestLabel "Given partial board, should update with central piece" $ updateBoard o partialBoard 4 ~?= Just partialBoardWithPiece
    ]

main =
    runTestTT tests
