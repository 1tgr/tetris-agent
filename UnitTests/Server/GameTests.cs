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

        private static void Test(ITetrisAgent agent)
        {
            string[] board = Boards.Empty.Split(' ');
            char[] pieceCodes = Pieces.All.Keys.ToArray();
            Random random = new Random();
            char pieceCode;
            IPiece rotatedPiece;
            TetrisMove move;

            do
            {
                Console.WriteLine(string.Join(Environment.NewLine, board));
                Console.WriteLine("----------");

                int pieceIndex = random.Next(pieceCodes.Length);
                pieceCode = pieceCodes[pieceIndex];
                IPiece piece = Pieces.All[pieceCode];

                move = agent.MovePiece(string.Join(" ", board), piece);
                Assert.GreaterOrEqual(move.Position, 0, "Position");
                Assert.LessOrEqual(move.Position, 10 - piece.Width, "Position");
                CollectionAssert.Contains(new[] { 0, 90, 180, 270 }, move.Degrees, "Degrees");

                rotatedPiece = Rotate(piece, move.Degrees);
            } while (rotatedPiece.TryUpdateBoard(board, move.Position, pieceCode));
        }

        [Test]
        public void RandomAgent()
        {
            Test(new RandomAgent(new Random(0)));
        }
    }
}