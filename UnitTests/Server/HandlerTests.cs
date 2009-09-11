using System.Web;
using NUnit.Framework;
using Rhino.Mocks;
using Tim.Tetris.Server;

namespace Tim.Tetris.UnitTests.Server
{
    [TestFixture]
    public class HandlerTests
    {
        [Test]
        public void ShouldProcessRequest()
        {
            MockRepository mocks = new MockRepository();
            HttpContextBase context = mocks.CreateMock<HttpContextBase>();

            using (mocks.Record())
            {
            }

            Handler handler = new Handler();
            handler.ProcessRequest(context);
            mocks.VerifyAll();
        }
    }
}