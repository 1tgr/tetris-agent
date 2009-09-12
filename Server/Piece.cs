namespace Tim.Tetris.Server
{
    public class Piece : IPiece
    {
        private readonly int[,] array;

        public Piece(int[,] array)
        {
            this.array = array;
        }

        /// <summary>
        /// rotate the piece array around
        /// </summary>
        private static T[,] TransposeArray<T>(T[,] x)
        {
            T[,] result = new T[x.GetUpperBound(1) + 1, x.GetUpperBound(0) + 1];

            for (int i = 0; i <= x.GetUpperBound(0); i++)
            {
                for (int j = 0; j <= x.GetUpperBound(1); j++)
                    result[j, i] = x[i, j];
            }

            return result;
        }

        public IPiece Transpose()
        {
            return new Piece(TransposeArray(array));
        }

        public int Width
        {
            get { return array.GetUpperBound(1) + 1; }
        }
    }
}