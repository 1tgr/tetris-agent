using System;

namespace Tim.Tetris.Server
{
    public class RandomPlayer : IPlayer
    {
        private readonly Random random;

        public RandomPlayer(Random random)
        {
            this.random = random;
        }

        public TetrisMove MovePiece(IBoard board, Piece piece)
        {
            int rand = random.Next(Board.DegreesOptions.Length);
            int degrees = Board.DegreesOptions[rand];
            int[,] pieceData = Board.Rotate(PieceData.All[piece], degrees);
            int position = FindDeepestColumn(board, pieceData.GetLength(1));
            return new TetrisMove(position, degrees);
        }

        private static int FindDeepestColumn(IBoard board, int pieceWidth)
        {
            int[] depths = Board.GetDepths(board);

            int deepestColumn = 0;
            int deepestOuter = 0;
            for (int column = 0; column <= Board.Width - pieceWidth; column++)
            {
                int shallowestInner = board.Height + 1;
                for (int pieceColumn = 0; pieceColumn < pieceWidth; pieceColumn++)
                {
                    if (depths[column + pieceColumn] < shallowestInner)
                        shallowestInner = depths[column + pieceColumn];
                }

                if (shallowestInner > deepestOuter)
                {
                    deepestOuter = shallowestInner;
                    deepestColumn = column;
                }
            }

            return deepestColumn;
        }
    }
}