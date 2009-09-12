namespace Tim.Tetris.Server
{
    public interface IPiece
    {
        IPiece Transpose();
        bool TryUpdateBoard(string[] board, int position, char code);
        int Width { get; }
    }
}