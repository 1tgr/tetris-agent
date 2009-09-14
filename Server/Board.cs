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

        private readonly string[] array;

        public Board(params string[] array)
        {
            this.array = array;
        }

        public static Board Parse(string code)
        {
            return new Board(code.Split(' '));
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

        private static void Collapse(string[] board, int firstRow, int rows)
        {
            Array.Copy(board, 0, board, rows, firstRow);
            for (int j = 0; j < rows; j++)
                board[j] = "..........";
        }

        public IBoard Collapse()
        {
            string[] newArray = (string[]) array.Clone();

            int firstRow = 0;
            int collapsedRows = 0;

            for (int row = 0; row < newArray.Length; row++)
            {
                if (newArray[row].Contains("."))
                {
                    Collapse(newArray, firstRow, collapsedRows);
                    collapsedRows = 0;
                }
                else
                {
                    if (collapsedRows == 0)
                        firstRow = row;

                    collapsedRows++;
                }
            }

            Collapse(newArray, newArray.Length, collapsedRows);
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