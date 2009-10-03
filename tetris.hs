import HUnit

type Board = [ String ]
type Piece = [ String ]

emptyRow = replicate 10 '.'
emptyBoard = replicate 20 emptyRow
emptyBoardWithPiece = (replicate 18 emptyRow) ++ [ "....oo....", "....oo...." ]
fullRow = replicate 10 'I'
fullBoard = replicate 20 fullRow
partialRow = "ssss..ssss"
partialBoard = (replicate 10 emptyRow) ++ [ partialRow ] ++ (replicate 9 fullRow)
partialBoardWithPiece = (replicate 9 emptyRow) ++ [ "....oo....", "ssssoossss" ] ++ (replicate 9 fullRow)
o = [ "oo", "oo" ]

collapse :: Board -> Board
collapse board =
    collapseInner [ ] board
    where
        collapseInner prior (x : xs) =
            if any (== '.') x then
                collapseInner (x : prior) xs
            else
                collapseInner (prior ++ [ emptyRow ]) xs

        collapseInner prior [ ] = 
            reverse prior

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

givenEmptyBoard_CollapseShouldProduceEmptyBoard = collapse emptyBoard @?= emptyBoard
givenFullBoard_CollapseShouldProduceEmptyBoard = collapse fullBoard @?= emptyBoard
givenPartialBoard_CollapseShouldRemoveFullRows = collapse partialBoard @?= (replicate 19 emptyRow) ++ [ partialRow ]
givenEmptyBoard_PieceShouldDropToBottom = dropPiece o emptyBoard 0 @?= Just 19
givenFullBoard_PieceShouldNotDrop = dropPiece o fullBoard 0 @?= Nothing
givenPartialBoard_CentralPieceShouldDropHalfway = dropPiece o partialBoard 4 @?= Just 10
givenPartialBoard_OffsetPieceShouldDropHalfway = dropPiece o partialBoard 3 @?= Just 9
givenPartialBoard_LeftPieceShouldDropHalfway = dropPiece o partialBoard 0 @?= Just 9
givenFullBoard_ShouldNotUpdate = updateBoard o fullBoard 4 @?= Nothing
givenEmptyBoard_ShouldUpdateWithCentralPiece = updateBoard o emptyBoard 4 @?= Just emptyBoardWithPiece
givenPartialBoard_ShouldUpdateWithCentralPiece = updateBoard o partialBoard 4 @?= Just partialBoardWithPiece

tests = 
    TestList
    [
        TestLabel "Given empty board, collapse should produce empty board" $ TestCase givenEmptyBoard_CollapseShouldProduceEmptyBoard,
        TestLabel "Given full board, collapse should produce empty board" $ TestCase givenFullBoard_CollapseShouldProduceEmptyBoard,
        TestLabel "Given partial board, collapse should remove rull rows" $ TestCase givenPartialBoard_CollapseShouldRemoveFullRows,
        TestLabel "Given empty board, piece should drop to bottom" $ TestCase givenEmptyBoard_PieceShouldDropToBottom,
        TestLabel "Given full board, piece should not drop" $ TestCase givenFullBoard_PieceShouldNotDrop,
        TestLabel "Given partial board, central piece should drop halfway" $ TestCase givenPartialBoard_CentralPieceShouldDropHalfway,
        TestLabel "Given partial board, offset piece should drop halfway" $ TestCase givenPartialBoard_OffsetPieceShouldDropHalfway,
        TestLabel "Given partial board, left piece should drop halfway" $ TestCase givenPartialBoard_LeftPieceShouldDropHalfway,
        TestLabel "Given full board, should not update" $ TestCase givenFullBoard_ShouldNotUpdate,
        TestLabel "Given empty board, should update with central piece" $ TestCase givenEmptyBoard_ShouldUpdateWithCentralPiece,
        TestLabel "Given partial board, should update with central piece" $ TestCase givenPartialBoard_ShouldUpdateWithCentralPiece
    ]

main =
    runTestTT tests
