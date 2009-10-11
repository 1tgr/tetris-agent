module Tetris.Engine where

import Ix  
import Random
import Tetris.BoundedEnum

type Board = [ String ]
type Piece = [ String ]

data PieceCode = I
               | J
               | L
               | O
               | S
               | T
               | Z
               deriving (Bounded, Enum, Eq, Ix, Ord, Show)

data Rotation = None
              | Ninety
              | OneEighty
              | TwoSeventy
              deriving (Bounded, Enum, Show)

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
