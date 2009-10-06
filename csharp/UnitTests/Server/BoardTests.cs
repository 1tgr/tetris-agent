using System.Linq;
using NUnit.Framework;
using Tim.Tetris.Server;

namespace Tim.Tetris.UnitTests.Server
{
    [TestFixture]
    public class BoardTests
    {
        [Test]
        public void GivenEmptyBoard_CollapseShouldProduceEmptyBoard()
        {
            Board board = Board.Empty;
            int score;
            Assert.AreEqual(Board.Empty, board.Collapse(out score));
            Assert.AreEqual(0, score, "Score");
        }

        [Test]
        public void GivenFullBoard_CollapseShouldProduceEmptyBoard()
        {
            Board board = new Board(Enumerable.Repeat(new string('I', Board.Width), 20).ToArray());
            int score;
            Assert.AreEqual(Board.Empty, board.Collapse(out score));
            Assert.AreEqual(20, score, "Score");
        }
    }
}