using System.Collections.Specialized;
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
            HttpContextBase context = MockRepository.GenerateMock<HttpContextBase>();
            ITetrisAgent agent = MockRepository.GenerateMock<ITetrisAgent>();
            HttpResponseBase response = MockRepository.GenerateMock<HttpResponseBase>();

            {
                const string board = ".......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... zzzzzzzz..";
                const string piece = "l";

                context.Stub(c => c.Response).Return(response);

                HttpRequestBase request = MockRepository.GenerateMock<HttpRequestBase>();
                context.Stub(c => c.Request).Return(request);
                request.Stub(r => r["piece"]).Return(piece);

                NameValueCollection form = MockRepository.GenerateMock<NameValueCollection>();
                request.Stub(r => r.Form).Return(form);
                form.Stub(f => f.Get("board")).Return(board);

                TetrisMove move = new TetrisMove(8, 0);
                agent.Expect(a => a.GetNextMove(piece, board)).Return(move);
            }

            Handler.ProcessRequest(context, agent);

            response.AssertWasCalled(r => r.ContentType = "text/plain");
            response.AssertWasCalled(r => r.Write("position=8&degrees=0"));
        }
    }
}