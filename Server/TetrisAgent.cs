using System;

namespace Tim.Tetris.Server
{
    public class TetrisAgent : ITetrisAgent
    {
        private static readonly int[] degreesOptions = new[] { 0, 90, 180, 270 };
        private const int BoardWidth = 10;
        private readonly Random random;

        public TetrisAgent(Random random)
        {
            this.random = random;
        }

        public TetrisMove MovePiece(string board, IPiece piece)
        {
            int rand = random.Next(3);

            for (int i = 0; i < rand; i++)
                piece = piece.Transpose();

            int position = FindLowestStackX(board.Split(' '), piece.Width);
            int degrees = degreesOptions[rand];
            return new TetrisMove(position, degrees);
        }

        /// <summary>
        /// find x coordinate of the lowest group of spaces on the board
        /// </summary>
        private static int FindLowestStackX(string[] board, int width)
        {
            int[] heights = new int[BoardWidth];

            Array.Reverse(board);
            for (int i = 0; i < board.Length; i++)
            {
                for (int j = 0; j < BoardWidth; j++)
                {
                    if (board[i][j] != '.')
                        heights[j] = i + 1;
                }
            }

            int pos = 0;
            int lowest = Int32.MaxValue;
            for (int x = 0; x <= BoardWidth - width; x++)
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