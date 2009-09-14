using System.Collections.Generic;

namespace Tim.Tetris.Server
{
    public static class Pieces
    {
        public static readonly Piece I = new Piece(new int[4, 1] { { 1 }, { 1 }, { 1 }, { 1 } });
        public static readonly Piece J = new Piece(new int[3, 2] { { 0, 1 }, { 0, 1 }, { 1, 1 } });
        public static readonly Piece L = new Piece(new int[3, 2] { { 1, 0 }, { 1, 0 }, { 1, 1 } });
        public static readonly Piece O = new Piece(new int[2, 2] { { 1, 1 }, { 1, 1 } });
        public static readonly Piece S = new Piece(new int[2, 3] { { 0, 1, 1 }, { 1, 1, 0 } });
        public static readonly Piece T = new Piece(new int[2, 3] { { 1, 1, 1 }, { 0, 1, 0 } });
        public static readonly Piece Z = new Piece(new int[2, 3] { { 1, 1, 0 }, { 0, 1, 1 } });

        public static readonly Dictionary<char, Piece> All =
            new Dictionary<char, Piece>
                {
                    { 'i', I },
                    { 'j', J },
                    { 'l', L },
                    { 'o', O },
                    { 's', S },
                    { 't', T },
                    { 'z', Z }
                };
    }
}