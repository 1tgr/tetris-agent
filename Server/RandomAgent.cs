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

        private static int[] GetHeights(string[] board)
        {
            int[] heights = new int[BoardWidth];

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
            int[] heights = GetHeights(board);

            int pos = 0;
            int lowest = Int32.MaxValue;
            for (int x = 0; x < BoardWidth - width; x++)
            {
                int highest = 0;
                for (int j = 0; j < width; j++)
                {
                    if (heights[x + j] > highest)
                        highest = heights[x + j];
                }

                if (highest < lowest)
                {
                    lowest = highest;
                    pos = x;
                }
            }

            return pos;
        }
    }
}