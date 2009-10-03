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

collided :: Piece -> Board -> Int -> Maybe Int
collided piece board position =
    collidedInner 0 $ map (drop position) $ board
    where
        collidedInner row boardInner @ (_ : bs) =
            if rowsCollided piece boardInner then
                Just row
            else
                collidedInner (row + 1) bs
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

        collidedInner row [ ] = Just (row - length piece + 1)

updateBoard :: Piece -> Board -> Int -> Maybe Board
updateBoard piece board position = do
    row <- collided piece board position
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
givenEmptyBoard_PieceShouldCollideAtBottom = collided o emptyBoard 0 @?= Just 19
givenFullBoard_PieceShouldCollide = collided o fullBoard 0 @?= Just 0
givenPartialBoard_CentralPieceShouldCollide = collided o partialBoard 4 @?= Just 10
givenPartialBoard_OffsetPieceShouldCollide = collided o partialBoard 3 @?= Just 9
givenPartialBoard_LeftPieceShouldCollide = collided o partialBoard 0 @?= Just 9
givenEmptyBoard_ShouldUpdateWithCentralPiece = updateBoard o emptyBoard 4 @?= Just emptyBoardWithPiece
givenPartialBoard_ShouldUpdateWithCentralPiece = updateBoard o partialBoard 4 @?= Just partialBoardWithPiece

tests = 
    TestList
    [
        TestLabel "Given empty board, collapse should produce empty board" $ TestCase givenEmptyBoard_CollapseShouldProduceEmptyBoard,
        TestLabel "Given full board, collapse should produce empty board" $ TestCase givenFullBoard_CollapseShouldProduceEmptyBoard,
        TestLabel "Given partial board, collapse should remove rull rows" $ TestCase givenPartialBoard_CollapseShouldRemoveFullRows,
        TestLabel "Given empty board, piece should collide at bottom" $ TestCase givenEmptyBoard_PieceShouldCollideAtBottom,
        TestLabel "Given full board, piece should collide" $ TestCase givenFullBoard_PieceShouldCollide,
        TestLabel "Given partial board, central piece should collide" $ TestCase givenPartialBoard_CentralPieceShouldCollide,
        TestLabel "Given partial board, offset piece should collide" $ TestCase givenPartialBoard_OffsetPieceShouldCollide,
        TestLabel "Given partial board, left piece should collide" $ TestCase givenPartialBoard_LeftPieceShouldCollide,
        TestLabel "Given empty board, should update with central piece" $ TestCase givenEmptyBoard_ShouldUpdateWithCentralPiece,
        TestLabel "Given partial board, should update with central piece" $ TestCase givenPartialBoard_ShouldUpdateWithCentralPiece
    ]

main =
    runTestTT tests
