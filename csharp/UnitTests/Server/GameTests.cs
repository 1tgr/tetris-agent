﻿using System;
using NUnit.Framework;
using Tim.Tetris.Server;

namespace Tim.Tetris.UnitTests.Server
{
    [TestFixture]
    public class GameTests
    {
        private static readonly Piece[] Pieces = (Piece[]) Enum.GetValues(typeof(Piece));

        private static void Test(IPlayer player, int count)
        {
            Random random = new Random();

            for (int game = 0; game < count; game++)
            {
                IBoard board = Board.Empty;
                Piece pieceCode;
                int[,] rotatedPieceData;
                TetrisMove move;

                do
                {
                    int score;
                    board = board.Collapse(out score);

                    Console.WriteLine(board);
                    Console.WriteLine("---------- {0}", score);

                    int pieceIndex = random.Next(Pieces.Length);
                    pieceCode = Pieces[pieceIndex];

                    try
                    {
                        move = player.MovePiece(board, pieceCode);
                        CollectionAssert.Contains(Board.DegreesOptions, move.Degrees, "Degrees");
                        Assert.GreaterOrEqual(move.Position, 0, "Position");

                        int[,] pieceData = PieceData.All[pieceCode];
                        rotatedPieceData = Board.Rotate(pieceData, move.Degrees);
                        Assert.LessOrEqual(move.Position, 10 - rotatedPieceData.GetLength(1), "Position");
                    }
                    catch (Exception ex)
                    {
                        throw new AssertionException(ex.Message + " Piece is " + pieceCode + ".", ex);
                    }
                } while (Board.TryUpdateBoard(rotatedPieceData, ref board, move.Position, char.ToLower(pieceCode.ToString()[0])));
            }
        }

        [Test]
        public void RandomGame()
        {
            Test(new RandomPlayer(new Random(0)), 1);
        }

        [Test, Explicit]
        public void TreeGame()
        {
            Test(new TreePlayer(new Random(0)), 1);
        }
    }
}