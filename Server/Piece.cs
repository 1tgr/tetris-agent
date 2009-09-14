using System.Linq;

namespace Tim.Tetris.Server
{
    public class Piece : IPiece
    {
        private readonly int[,] array;

        public Piece(int[,] array)
        {
            this.array = array;
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != typeof(Piece)) return false;

            Piece other = (Piece) obj;
            if (other.array.GetLength(0) != array.GetLength(0))
                return false;

            if (other.array.GetLength(1) != array.GetLength(1))
                return false;

            return other.array.Cast<int>().SequenceEqual(array.Cast<int>());
        }

        public override int GetHashCode()
        {
            return array.Cast<int>().Aggregate(0, (hashCode, item) => (hashCode * 29) + item.GetHashCode());
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

        private bool DoCollision(ref IBoard board, int row, int column, char? code)
        {
            for (int pieceColumn = 0; pieceColumn < array.GetLength(1); pieceColumn++)
            {
                for (int pieceRow = 0; pieceRow < array.GetLength(0); pieceRow++)
                {
                    if (array[pieceRow, pieceColumn] == 0)
                        continue;

                    string boardCode = board[row + pieceRow];
                    if (code == null)
                    {
                        if (boardCode[column + pieceColumn] != '.')
                            return true;
                    }
                    else
                    {
                        string newBoardCode = boardCode.Substring(0, column + pieceColumn) + code + boardCode.Substring(column + pieceColumn + 1);
                        board = board.Update(row + pieceRow, newBoardCode);
                    }
                }
            }

            return false;
        }

        public bool TryUpdateBoard(ref IBoard board, int position, char code)
        {
            for (int row = board.Height - array.GetLength(0); row >= 0; row--)
            {
                if (DoCollision(ref board, row, position, null))
                    continue;

                DoCollision(ref board, row, position, code);
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