using System;
using System.Linq;
using NUnit.Framework;
using Tim.Tetris.Server;

namespace Tim.Tetris.UnitTests.Server
{
    [TestFixture]
    public class GameTests
    {
        private static IPiece Rotate(IPiece piece, int degrees)
        {
            switch (degrees)
            {
                case 270:
                    piece = piece.Transpose();
                    goto case 180;

                case 180:
                    piece = piece.Transpose();
                    goto case 90;

                case 90:
                    piece = piece.Transpose();
                    break;
            }

            return piece;
        }

        private static void Test(IPlayer player, int count)
        {
            char[] pieceCodes = Pieces.All.Keys.ToArray();
            Random random = new Random();

            for (int game = 0; game < count; game++)
            {
                IBoard board = Board.Empty;
                char pieceCode;
                IPiece rotatedPiece;
                TetrisMove move;

                do
                {
                    board = board.Collapse();

                    Console.WriteLine(board);
                    Console.WriteLine("----------");

                    int pieceIndex = random.Next(pieceCodes.Length);
                    pieceCode = pieceCodes[pieceIndex];

                    try
                    {
                        IPiece piece = Pieces.All[pieceCode];

                        move = player.MovePiece(board, piece);
                        CollectionAssert.Contains(new[] { 0, 90, 180, 270 }, move.Degrees, "Degrees");
                        Assert.GreaterOrEqual(move.Position, 0, "Position");

                        rotatedPiece = Rotate(piece, move.Degrees);
                        Assert.LessOrEqual(move.Position, 10 - rotatedPiece.Width, "Position");
                    }
                    catch (Exception ex)
                    {
                        throw new AssertionException(ex.Message + " Piece is " + char.ToUpper(pieceCode) + ".", ex);
                    }
                } while (rotatedPiece.TryUpdateBoard(ref board, move.Position, pieceCode));
            }
        }

        [Test]
        public void RandomAgent()
        {
            Test(new RandomPlayer(new Random(0)), 1);
        }
    }
}