using System;
using NUnit.Framework;
using Rhino.Mocks;
using Tim.Tetris.Server;

namespace Tim.Tetris.UnitTests.Server
{
    [TestFixture]
    public class TetrisAgentTests
    {
        [Test]
        public void ShouldGetNextMove()
        {
            Random random = MockRepository.GenerateStrictMock<Random>();
            TetrisAgent agent = new TetrisAgent(random);
            random.Stub(r => r.Next(3)).Return(0);
            Assert.AreEqual(new TetrisMove(8, 0), agent.GetNextMove(Consts.Piece, Consts.Board));
        }
    }
}