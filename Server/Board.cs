using System;
using System.Linq;

namespace Tim.Tetris.Server
{
    public class Board : IBoard
    {
        public static readonly Board Empty =
            new Board(
                "..........",
                "..........",
                "..........",
                "..........",
                "..........",
                "..........",
                "..........",
                "..........",
                "..........",
                "..........",
                "..........",
                "..........",
                "..........",
                "..........",
                "..........",
                "..........",
                "..........",
                "..........",
                "..........",
                "..........");

        public const int Width = 10;
        public static readonly int[] DegreesOptions = new[] { 0, 90, 180, 270 };
        private readonly string[] array;

        public Board(params string[] array)
        {
            this.array = array;
        }

        public static Board Parse(string code)
        {
            return new Board(code.Split(' '));
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

        public static int[,] Rotate(int[,] pieceData, int degrees)
        {
            switch (degrees)
            {
                case 270:
                    pieceData = TransposeArray(pieceData);
                    goto case 180;

                case 180:
                    pieceData = TransposeArray(pieceData);
                    goto case 90;

                case 90:
                    pieceData = TransposeArray(pieceData);
                    break;
            }

            return pieceData;
        }

        private static bool DoCollision(int[,] pieceData, ref IBoard board, int row, int column, char? code)
        {
            for (int pieceColumn = 0; pieceColumn < pieceData.GetLength(1); pieceColumn++)
            {
                for (int pieceRow = 0; pieceRow < pieceData.GetLength(0); pieceRow++)
                {
                    if (pieceData[pieceRow, pieceColumn] == 0)
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

        public static bool TryUpdateBoard(int[,] pieceData, ref IBoard board, int position, char code)
        {
            for (int row = board.Height - pieceData.GetLength(0); row >= 0; row--)
            {
                if (DoCollision(pieceData, ref board, row, position, null))
                    continue;

                DoCollision(pieceData, ref board, row, position, code);
                return true;
            }

            return false;
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != typeof(Board)) return false;
            Board other = (Board) obj;
            return other.array.SequenceEqual(array);
        }

        public override int GetHashCode()
        {
            return array.Aggregate(0, (hashCode, item) => (hashCode * 29) + item.GetHashCode());
        }

        public override string ToString()
        {
            return String.Join(Environment.NewLine, array);
        }

        private int Collapse(ref string[] board, int firstRow, int rows)
        {
            if (rows == 0)
                return 0;

            if (board == array)
                board = (string[]) array.Clone();

            Array.Copy(board, 0, board, rows, Math.Min(firstRow, board.Length - rows));
            for (int j = 0; j < rows; j++)
                board[j] = "..........";

            return rows;
        }

        public IBoard Collapse(out int score)
        {
            string[] newArray = array;
            int firstRow = 0;
            int collapsedRows = 0;
            score = 0;

            for (int row = 0; row < newArray.Length; row++)
            {
                if (newArray[row].Contains("."))
                {
                    score += Collapse(ref newArray, firstRow, collapsedRows);
                    collapsedRows = 0;
                }
                else
                {
                    if (collapsedRows == 0)
                        firstRow = row;

                    collapsedRows++;
                }
            }

            score += Collapse(ref newArray, newArray.Length, collapsedRows);
            return new Board(newArray);
        }

        public IBoard Update(int row, string code)
        {
            string[] newArray = (string[]) array.Clone();
            newArray[row] = code;
            return new Board(newArray);
        }

        public string this[int row]
        {
            get { return array[row]; }
        }

        public int Height
        {
            get { return array.Length; }
        }
    }
}