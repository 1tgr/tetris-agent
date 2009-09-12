﻿using System;
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
            Random random = MockRepository.GenerateMock<Random>();
            HttpResponseBase response = MockRepository.GenerateMock<HttpResponseBase>();

            {
                context.Stub(c => c.Response).Return(response);

                HttpRequestBase request = MockRepository.GenerateMock<HttpRequestBase>();
                context.Stub(c => c.Request).Return(request);
                request.Stub(r => r["piece"]).Return("l");

                NameValueCollection form = MockRepository.GenerateMock<NameValueCollection>();
                request.Stub(r => r.Form).Return(form);
                form.Stub(f => f.Get("board")).Return(".......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... zzzzzzzz..");

                random.Expect(r => r.Next(3)).Return(0);
            }

            Handler.ProcessRequest(context, random);

            response.AssertWasCalled(r => r.ContentType = "text/plain");
            response.AssertWasCalled(r => r.Write("position=8&degrees=0"));
        }
    }
}