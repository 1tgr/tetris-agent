import HUnit

emptyRow = replicate 10 '.'
emptyBoard = replicate 20 emptyRow
fullRow = replicate 10 'I'
fullBoard = replicate 20 fullRow
partialRow = "ssss..ssss"
partialBoard = (replicate 10 emptyRow) ++ [ partialRow ] ++ (replicate 9 fullRow)
o = [ "oo", "oo" ]

collapse :: [ String ] -> [ String ]
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

collided :: [ String ] -> [ String ] -> Int -> Maybe Int
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

        collidedInner _ [ ] = Nothing

givenEmptyBoard_CollapseShouldProduceEmptyBoard = collapse emptyBoard @?= emptyBoard
givenFullBoard_CollapseShouldProduceEmptyBoard = collapse fullBoard @?= emptyBoard
givenPartialBoard_CollapseShouldRemoveFullRows = collapse partialBoard @?= (replicate 19 emptyRow) ++ [ partialRow ]
givenEmptyBoard_PieceShouldNotCollide = collided o emptyBoard 0 @?= Nothing
givenFullBoard_PieceShouldCollide = collided o fullBoard 0 @?= Just 0
givenPartialBoard_CentralPieceShouldCollide = collided o partialBoard 4 @?= Just 10
givenPartialBoard_OffsetPieceShouldCollide = collided o partialBoard 3 @?= Just 9
givenPartialBoard_LeftPieceShouldCollide = collided o partialBoard 0 @?= Just 9

tests = 
    TestList
    [
        TestLabel "Given empty board, collapse should produce empty board" $ TestCase givenEmptyBoard_CollapseShouldProduceEmptyBoard,
        TestLabel "Given full board, collapse should produce empty board" $ TestCase givenFullBoard_CollapseShouldProduceEmptyBoard,
        TestLabel "Given partial board, collapse should remove rull rows" $ TestCase givenPartialBoard_CollapseShouldRemoveFullRows,
        TestLabel "Given empty board, piece should not collide" $ TestCase givenEmptyBoard_PieceShouldNotCollide,
        TestLabel "Given full board, piece should collide" $ TestCase givenFullBoard_PieceShouldCollide,
        TestLabel "Given partial board, central piece should collide" $ TestCase givenPartialBoard_CentralPieceShouldCollide,
        TestLabel "Given partial board, offset piece should collide" $ TestCase givenPartialBoard_OffsetPieceShouldCollide,
        TestLabel "Given partial board, left piece should collide" $ TestCase givenPartialBoard_LeftPieceShouldCollide
    ]

main =
    runTestTT tests
