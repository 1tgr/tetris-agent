using System;

namespace Tim.Tetris.Server
{
    public class RandomAgent : ITetrisAgent
    {
        private static readonly int[] degreesOptions = new[] { 0, 90, 180, 270 };
        private const int BoardWidth = 10;
        private readonly Random random;

        public RandomAgent(Random random)
        {
            this.random = random;
        }

        public TetrisMove MovePiece(string board, IPiece piece)
        {
            int rand = random.Next(degreesOptions.Length);

            for (int i = 0; i < rand; i++)
                piece = piece.Transpose();

            int position = FindLowestStackX(board.Split(' '), piece.Width);
            int degrees = degreesOptions[rand];
            return new TetrisMove(position, degrees);
        }

        private static int[] GetDepths(string[] board)
        {
            int[] heights = new int[BoardWidth];

            for (int j = 0; j < BoardWidth; j++)
                heights[j] = board.Length + 1;

            for (int i = board.Length - 1; i >= 0; i--)
            {
                for (int j = 0; j < BoardWidth; j++)
                {
                    if (board[i][j] != '.')
                        heights[j] = i + 1;
                }
            }

            return heights;
        }

        private static int FindLowestStackX(string[] board, int width)
        {
            int[] depths = GetDepths(board);

            int pos = 0;
            int deepestOuter = 0;
            for (int column = 0; column <= BoardWidth - width; column++)
            {
                int innerLimit = board.Length + 1;
                for (int pieceColumn = 0; pieceColumn < width; pieceColumn++)
                {
                    if (depths[column + pieceColumn] < innerLimit)
                        innerLimit = depths[column + pieceColumn];
                }

                if (innerLimit > deepestOuter)
                {
                    deepestOuter = innerLimit;
                    pos = column;
                }
            }

            return pos;
        }
    }
}