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

            int position = FindDeepestColumn(board.Split(' '), piece.Width);
            int degrees = degreesOptions[rand];
            return new TetrisMove(position, degrees);
        }

        private static int[] GetDepths(string[] board)
        {
            int[] heights = new int[BoardWidth];

            for (int column = 0; column < BoardWidth; column++)
                heights[column] = board.Length + 1;

            for (int row = board.Length - 1; row >= 0; row--)
            {
                for (int column = 0; column < BoardWidth; column++)
                {
                    if (board[row][column] != '.')
                        heights[column] = row + 1;
                }
            }

            return heights;
        }

        private static int FindDeepestColumn(string[] board, int pieceWidth)
        {
            int[] depths = GetDepths(board);

            int deepestColumn = 0;
            int deepestOuter = 0;
            for (int column = 0; column <= BoardWidth - pieceWidth; column++)
            {
                int shallowestInner = board.Length + 1;
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