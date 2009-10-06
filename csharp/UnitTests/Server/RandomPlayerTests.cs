using System;
using NUnit.Framework;
using Rhino.Mocks;
using Tim.Tetris.Server;

namespace Tim.Tetris.UnitTests.Server
{
    [TestFixture]
    public class RandomPlayerTests
    {
        [Test]
        public void WhenEmpty_ShouldPositionOToLeft()
        {
            Random random = MockRepository.GenerateStrictMock<Random>();
            RandomPlayer player = new RandomPlayer(random);
            random.Stub(r => r.Next(4)).Return(0);
            Assert.AreEqual(new TetrisMove(0, 0), player.MovePiece(Board.Empty, Piece.O));
        }

        [Test]
        public void WhenBottomLeftOccupied_ShouldPositionOToRight()
        {
            Random random = MockRepository.GenerateStrictMock<Random>();
            RandomPlayer player = new RandomPlayer(random);
            random.Stub(r => r.Next(4)).Return(0);
            Assert.AreEqual(new TetrisMove(8, 0), player.MovePiece(Boards.BottomLeftOccupied, Piece.O));
        }
    }
}