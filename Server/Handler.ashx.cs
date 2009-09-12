﻿using System;
using System.Collections.Generic;
using System.Web;
using System.Diagnostics;

namespace Tim.Tetris.Server
{
    /// <summary>
    /// Summary description for $codebehindclassname$
    /// </summary>
    public class Handler : IHttpHandler
    {
        private static readonly Dictionary<char, int[,]> pieces =
            new Dictionary<char, int[,]>
                {
                    { 'i', new[,] { { 1 }, { 1 }, { 1 }, { 1 } } },
                    { 'j', new[,] { { 0, 1 }, { 0, 1 }, { 1, 1 } } },
                    { 'l', new[,] { { 1, 0 }, { 1, 0 }, { 1, 1 } } },
                    { 'o', new[,] { { 1, 1 }, { 1, 1 } } },
                    { 's', new[,] { { 0, 1, 1 }, { 1, 1, 0 } } },
                    { 't', new[,] { { 1, 1, 1 }, { 0, 1, 0 } } },
                    { 'z', new[,] { { 1, 1, 0 }, { 0, 1, 1 } } }
                };

        private const int BoardWidth = 10;

        public void ProcessRequest(HttpContext context)
        {
            ProcessRequest(new HttpContextWrapper(context), new Random());
        }

        public static void ProcessRequest(HttpContextBase context, Random random)
        {
            string board = context.Request.Form["board"];//".......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... .......... zzzzzzzz..";
            string piece = context.Request["piece"]; //"l";  
            int[,] pieceArray = pieces[piece[0]];


            int[] degreesOptions = { 0, 90, 180, 270 };
            int rand = random.Next(3);
            int degrees = degreesOptions[rand];

            //rotate the piece a random number of times
            for (int i = 0; i < rand; i++)
            {
                pieceArray = TransposeArray(pieceArray);
            }

            int width = pieceArray.GetUpperBound(1) + 1;
            int position = FindLowestStackX(board.Split(' '), width);

            context.Response.ContentType = "text/plain";
            context.Response.Write(string.Format("position={0}&degrees={1}", position, degrees));
        }


        /// <summary>
        /// rotate the piece array around
        /// </summary>
        private static int[,] TransposeArray(int[,] x)
        {
            int[,] result = new int[x.GetUpperBound(1) + 1, x.GetUpperBound(0) + 1];
            int i, j;

            for (i = 0; i <= x.GetUpperBound(0); i++)
                for (j = 0; j <= x.GetUpperBound(1); j++)
                {
                    result[j, i] = x[i, j];
                }

            return result;
        }

        /// <summary>
        /// find x coordinate of the lowest group of spaces on the board
        /// </summary>
        private static int FindLowestStackX(string[] board, int width)
        {
            int[] heights = new int[BoardWidth];
            int pos = 0;
            int lowest = Int32.MaxValue;

            Array.Reverse(board);
            for (int i = 0; i < board.Length; i++)
            {
                for (int j = 0; j < BoardWidth; j++)
                {
                    if (board[i][j] != '.') heights[j] = i + 1;
                }
            }

            // find the group of 'width' spaces with the lowest max height
            for (int x = 0; x <= BoardWidth - width; x++)
            {
                int heighest = 0;
                for (int j = 0; j < width; j++)
                {
                    if (heights[x + j] > heighest) heighest = heights[x + j];
                }
                if (heighest < lowest)
                {
                    lowest = heighest;
                    pos = x;
                }
            }

            return pos;
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




