using System;
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
            Random random = mocks.CreateMock<Random>();

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
                Expect.Call(random.Next(3)).Return(0);
                response.ContentType = "text/plain";
                response.Write("position=8&degrees=0");
            }

            Handler.ProcessRequest(context, random);
            mocks.VerifyAll();
        }
    }
}