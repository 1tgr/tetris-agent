using System;
using NUnit.Framework;
using Rhino.Mocks;
using Tim.Tetris.Server;

namespace Tim.Tetris.UnitTests.Server
{
    [TestFixture]
    public class RandomAgentTests
    {
        [Test]
        public void WhenEmpty_ShouldPositionOToLeft()
        {
            Random random = MockRepository.GenerateStrictMock<Random>();
            RandomAgent agent = new RandomAgent(random);
            random.Stub(r => r.Next(4)).Return(0);
            Assert.AreEqual(new TetrisMove(0, 0), agent.MovePiece(Boards.Empty, Pieces.O));
        }

        [Test]
        public void WhenBottomLeftOccupied_ShouldPositionOToRight()
        {
            Random random = MockRepository.GenerateStrictMock<Random>();
            RandomAgent agent = new RandomAgent(random);
            random.Stub(r => r.Next(4)).Return(0);
            Assert.AreEqual(new TetrisMove(8, 0), agent.MovePiece(Boards.BottomLeftOccupied, Pieces.O));
        }
    }
}