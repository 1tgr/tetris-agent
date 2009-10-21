module Tetris.Engine where

import Control.Monad.State.Lazy
import Ix  
import Random
import Tetris.BoundedEnum()

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

emptyRow :: String
emptyRow = replicate 10 '.'

emptyBoard :: Board
emptyBoard = replicate 20 emptyRow

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

collapse :: Board -> Int -> (Board, Int)
collapse = collapseInner [ ]
    where collapseInner :: Board -> Board -> Int -> (Board, Int)
          collapseInner prior (x : xs) | any (== '.') x = collapseInner (x : prior) xs
                                       | otherwise = collapseInner (prior ++ [ emptyRow ]) xs . (+1)
          collapseInner prior [ ] = (,) $ reverse prior

dropPiece :: Piece -> Board -> Int -> Maybe Int
dropPiece piece board position =
    case dropPieceInner (-2) $ map (drop position) $ emptyRow : emptyRow : board of
    row | row >= 0 -> Just row
    _ -> Nothing
    where dropPieceInner row boardInner @ (_ : bs) | rowsCollided piece boardInner = row
                                                   | otherwise = dropPieceInner (row + 1) bs
          dropPieceInner row [ ] = row - length piece + 1

          rowsCollided (p : ps) (b : bs) = cellsCollided p b || rowsCollided ps bs
          rowsCollided [ ] _ = False
          rowsCollided _ [ ] = False

          cellsCollided (_ : pps) ('.' : bbs) = cellsCollided pps bbs
          cellsCollided ('.' : pps) (_ : bbs) = cellsCollided pps bbs
          cellsCollided [ ] _ = False
          cellsCollided _ _ = True

updateBoard :: Piece -> Board -> Int -> Maybe Board
updateBoard piece board position = do
    row <- dropPiece piece board position
    return $ updateBoardInner row
    where drawCell '.' bb = bb
          drawCell pp _ = pp

          drawPiece (p : ps) (b : bs) = (left ++ centre ++ right) : drawPiece ps bs
              where pieceWidth = length p
                    left = take position b
                    centre = zipWith drawCell p $ take pieceWidth $ drop position b
                    right = drop (position + pieceWidth) b

          drawPiece [ ] _ = [ ]
          drawPiece _ [ ] = [ ]

          updateBoardInner row = top ++ middle ++ bottom
              where top = take (row - 1) board
                    middle = drawPiece piece $ take pieceLength $ drop (row - 1) board
                    bottom = drop (row + pieceLength - 1) board
                    pieceLength = length piece

playGame :: RandomGen g => (PieceCode -> Board -> State a (Int, Rotation)) -> a -> State g (Int, Int, Board)
playGame player state = State { runState = \g -> evalState (playGameInner (0, 0, emptyBoard) g) state }
    where playGameInner (score, turns, board) g = do
            let (pieceCode, g') = random g
            (position, rotation) <- player pieceCode board
            let pieceData = rotatedPiece pieceCode rotation
            case updateBoard pieceData board position of
                Just board' -> 
                    let (board'', score') = collapse board' score in
                    playGameInner (score', turns + 1, board'') g'
                Nothing -> return ((score, turns, board), g')

depths :: Board -> [ Int ]
depths = depthsInner 0
    where depthsInner :: Int -> Board -> [ Int ]
          depthsInner _ [ ] = [ ]
          depthsInner row [ x ] = rowDepths row x
          depthsInner row (x : xs) = zipWith min (rowDepths row x) $ depthsInner (row + 1) xs
          rowDepths row = map (\c -> if c == '.' then 20 else row)
