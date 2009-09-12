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
            MockRepository mocks = new MockRepository();
            HttpContextBase context = mocks.CreateMock<HttpContextBase>();

            using (mocks.Record())
            {
                HttpRequestBase request = mocks.CreateMock<HttpRequestBase>();
                NameValueCollection form = mocks.CreateMock<NameValueCollection>();
                HttpResponseBase response = mocks.CreateMock<HttpResponseBase>();
                SetupResult.For(context.Request).Return(request);
                SetupResult.For(request.Form).Return(form);
                SetupResult.For(form.Get("board")).Return(".......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... zzzzzzzz..");
                SetupResult.For(request["piece"]).Return("l");
                SetupResult.For(context.Response).Return(response);
                response.ContentType = "text/plain";
                response.Write(null);
                LastCall.IgnoreArguments();
            }

            Handler handler = new Handler();
            handler.ProcessRequest(context);
            mocks.VerifyAll();
        }
    }
}