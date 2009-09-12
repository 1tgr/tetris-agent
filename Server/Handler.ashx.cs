using System;
using System.Web;

namespace Tim.Tetris.Server
{
    public class Handler : IHttpHandler
    {
        public void ProcessRequest(HttpContext context)
        {
            ProcessRequest(new HttpContextWrapper(context), new TetrisAgent(new Random()));
        }

        public static void ProcessRequest(HttpContextBase context, ITetrisAgent agent)
        {
            string board = context.Request.Form["board"];//".......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... zzzzzzzz..";
            string piece = context.Request["piece"]; //"l";  
            TetrisMove move = agent.MovePiece(board, Pieces.All[piece[0]]);
            context.Response.ContentType = "text/plain";
            context.Response.Write(string.Format("position={0}&degrees={1}", move.Position, move.Degrees));
        }

        public bool IsReusable
        {
            get
            {
                return false;
            }
        }
    }
}