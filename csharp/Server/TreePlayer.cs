using System;
using System.Collections.Generic;
using System.Linq;

namespace Tim.Tetris.Server
{
    public class TreePlayer : IPlayer
    {
        private readonly Random random;
        private static readonly Piece[] pieces = (Piece[])Enum.GetValues(typeof(Piece));

        public TreePlayer(Random random)
        {
            this.random = random;
        }

        private static IEnumerable<T> Shuffle<T>(IEnumerable<T> source, Random rng)
        {
            T[] elements = source.ToArray();
            // Note i > 0 to avoid final pointless iteration
            for (int i = elements.Length - 1; i > 0; i--)
            {
                // Swap element "i" with a random earlier element it (or itself)
                int swapIndex = rng.Next(i + 1);
                T tmp = elements[i];
                elements[i] = elements[swapIndex];
                elements[swapIndex] = tmp;
            }
            // Lazily yield (avoiding aliasing issues etc)
            foreach (T element in elements)
            {
                yield return element;
            }
        }

        private IEnumerable<int> PickPositions(int pieceWidth)
        {
            //return Shuffle(Enumerable.Range(0, Board.Width - pieceWidth), random).Take(1);
            return new int[] { random.Next(Board.Width - pieceWidth) };
        }

        private double RunGame(IBoard board, Piece piece, int[,] pieceData, int position, int depth)
        {
            if (!Board.TryUpdateBoard(pieceData, ref board, position, char.ToUpper(piece.ToString()[0])))
                return 0;

            int score;
            board = board.Collapse(out score);

            if (depth >= 4)
                return score;

            double averageScore =
                (from nextPiece in pieces
                 from nextDegrees in Board.DegreesOptions
                 let nextPieceData = Board.Rotate(PieceData.All[nextPiece], nextDegrees)
                 from nextPosition in PickPositions(nextPieceData.GetLength(1))
                 let nextScore = RunGame(board, nextPiece, nextPieceData, nextPosition, depth + 1)
                 select nextScore)
                    .Average();

            return score + averageScore;
        }

        public TetrisMove MovePiece(IBoard board, Piece piece)
        {
            return
                (from degrees in Board.DegreesOptions
                 let pieceData = Board.Rotate(PieceData.All[piece], degrees)
                 from position in PickPositions(pieceData.GetLength(1))
                 let score = RunGame(board, piece, pieceData, position, 0)
                 select new { Move = new TetrisMove(position, degrees), Score = score })
                    .OrderByDescending(t => t.Score)
                    .First()
                    .Move;
        }
    }
}