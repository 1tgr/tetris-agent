namespace Tim.Tetris.Server
{
    public class TetrisMove
    {
        public TetrisMove(int position, int degrees)
        {
            Position = position;
            Degrees = degrees;
        }

        public int Position { get; private set; }
        public int Degrees { get; private set; }
    }
}