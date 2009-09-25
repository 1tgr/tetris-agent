using System.Linq;
using Microsoft.FSharp.Core;
using NUnit.Framework;
using Tim.Tetris.Core;
using Tim.Tetris.Server;

namespace Tim.Tetris.UnitTests.Server
{
    [TestFixture]
    public class BoardTests
    {
        [Test]
        public void GivenEmptyBoard_CollapseShouldProduceEmptyBoard()
        {
            Game.Board board = Game.empty;
            Tuple<int, Game.Board> t = Game.collapse(board);
            Assert.AreEqual(0, t.Item1, "Score");
            Assert.AreEqual(Game.empty, t.Item2);
        }

        [Test]
        public void GivenFullBoard_CollapseShouldProduceEmptyBoard()
        {
            Game.Board board = Game.parse(string.Join(" ", Enumerable.Repeat(new string('I', Board.Width), 20).ToArray()));
            Tuple<int, Game.Board> t = Game.collapse(board);
            Assert.AreEqual(20, t.Item1, "Score");
            Assert.AreEqual(Game.empty, t.Item2);
        }
    }
}