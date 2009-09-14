using System;
using System.Web;

namespace Tim.Tetris.Server
{
    public class Handler : IHttpHandler
    {
        public void ProcessRequest(HttpContext context)
        {
            ProcessRequest(new HttpContextWrapper(context), new RandomPlayer(new Random()));
        }

        public static void ProcessRequest(HttpContextBase context, IPlayer player)
        {
            string boardCode = context.Request.Form["board"];//".......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... zzzzzzzz..";
            string pieceCode = context.Request["piece"]; //"l";  
            IBoard board = Board.Parse(boardCode);
            IPiece piece = Pieces.All[pieceCode[0]];
            TetrisMove move = player.MovePiece(board, piece);
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