namespace Tim.Tetris.Server
{
    public class Piece : IPiece
    {
        private readonly int[,] array;

        public Piece(int[,] array)
        {
            this.array = array;
        }

        private static T[,] TransposeArray<T>(T[,] x)
        {
            T[,] result = new T[x.GetLength(1), x.GetLength(0)];

            for (int i = 0; i < x.GetLength(0); i++)
            {
                for (int j = 0; j < x.GetLength(1); j++)
                    result[j, i] = x[i, j];
            }

            return result;
        }

        public IPiece Transpose()
        {
            return new Piece(TransposeArray(array));
        }

        private bool DoCollision(string[] board, int row, int column, char? code)
        {
            for (int pieceColumn = 0; pieceColumn < array.GetLength(0); pieceColumn++)
            {
                for (int pieceRow = 0; pieceRow < array.GetLength(1); pieceRow++)
                {
                    if (array[pieceColumn, pieceRow] == 0)
                        continue;

                    string s = board[row + pieceRow];
                    if (code == null)
                    {
                        if (s[column + pieceColumn] != '.')
                            return true;
                    }
                    else
                        board[row + pieceRow] = s.Substring(0, column + pieceColumn) + code + s.Substring(column + pieceColumn + 1);
                }
            }

            return false;
        }

        public bool TryUpdateBoard(string[] board, int position, char code)
        {
            for (int row = board.Length - array.GetLength(1); row >= 0; row--)
            {
                if (DoCollision(board, row, position, null))
                    continue;

                DoCollision(board, row, position, code);
                return true;
            }

            return false;
        }

        public int Width
        {
            get { return array.GetLength(1); }
        }
    }
}