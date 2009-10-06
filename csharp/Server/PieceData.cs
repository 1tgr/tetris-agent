using System.Collections.Generic;

namespace Tim.Tetris.Server
{
    public static class PieceData
    {
        public static readonly int[,] I = new int[4, 1] { { 1 }, { 1 }, { 1 }, { 1 } };
        public static readonly int[,] J = new int[3, 2] { { 0, 1 }, { 0, 1 }, { 1, 1 } };
        public static readonly int[,] L = new int[3, 2] { { 1, 0 }, { 1, 0 }, { 1, 1 } };
        public static readonly int[,] O = new int[2, 2] { { 1, 1 }, { 1, 1 } };
        public static readonly int[,] S = new int[2, 3] { { 0, 1, 1 }, { 1, 1, 0 } };
        public static readonly int[,] T = new int[2, 3] { { 1, 1, 1 }, { 0, 1, 0 } };
        public static readonly int[,] Z = new int[2, 3] { { 1, 1, 0 }, { 0, 1, 1 } };

        public static readonly Dictionary<Piece, int[,]> All =
            new Dictionary<Piece, int[,]>
                {
                    { Piece.I, I },
                    { Piece.J, J },
                    { Piece.L, L },
                    { Piece.O, O },
                    { Piece.S, S },
                    { Piece.T, T },
                    { Piece.Z, Z }
                };
    }
}