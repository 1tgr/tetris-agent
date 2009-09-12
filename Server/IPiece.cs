namespace Tim.Tetris.Server
{
    public interface IPiece
    {
        IPiece Transpose();
        int Width { get; }
    }
}