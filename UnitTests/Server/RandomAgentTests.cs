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
        public void WhenEmpty_ShouldPositionLToLeft()
        {
            Random random = MockRepository.GenerateStrictMock<Random>();
            RandomAgent agent = new RandomAgent(random);
            random.Stub(r => r.Next(3)).Return(0);
            Assert.AreEqual(new TetrisMove(0, 0), agent.MovePiece(Boards.Empty, Pieces.L));
        }

        [Test]
        public void WhenBottomLeftOccupied_ShouldPositionLToRight()
        {
            Random random = MockRepository.GenerateStrictMock<Random>();
            RandomAgent agent = new RandomAgent(random);
            random.Stub(r => r.Next(3)).Return(0);
            Assert.AreEqual(new TetrisMove(8, 0), agent.MovePiece(Boards.BottomLeftOccupied, Pieces.L));
        }
    }
}