namespace Tim.Tetris.Server
{
    public class TetrisMove
    {
        public TetrisMove(int position, int degrees)
        {
            Position = position;
            Degrees = degrees;
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != typeof(TetrisMove)) return false;
            TetrisMove other = (TetrisMove) obj;
            return other.Position == Position && other.Degrees == Degrees;
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return (Position * 397) ^ Degrees;
            }
        }

        public int Position { get; private set; }
        public int Degrees { get; private set; }
    }
}