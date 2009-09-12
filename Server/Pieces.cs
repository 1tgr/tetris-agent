using System.Collections.Generic;

namespace Tim.Tetris.Server
{
    public static class Pieces
    {
        public static readonly IPiece I = new Piece(new int[4, 1] { { 1 }, { 1 }, { 1 }, { 1 } });
        public static readonly IPiece J = new Piece(new int[3, 2] { { 0, 1 }, { 0, 1 }, { 1, 1 } });
        public static readonly IPiece L = new Piece(new int[3, 2] { { 1, 0 }, { 1, 0 }, { 1, 1 } });
        public static readonly IPiece O = new Piece(new int[2, 2] { { 1, 1 }, { 1, 1 } });
        public static readonly IPiece S = new Piece(new int[2, 3] { { 0, 1, 1 }, { 1, 1, 0 } });
        public static readonly IPiece T = new Piece(new int[2, 3] { { 1, 1, 1 }, { 0, 1, 0 } });
        public static readonly IPiece Z = new Piece(new int[2, 3] { { 1, 1, 0 }, { 0, 1, 1 } });

        public static readonly IDictionary<char, IPiece> All =
            new Dictionary<char, IPiece>
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