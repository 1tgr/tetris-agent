namespace Tim.Tetris.Server
{
    public interface IPiece
    {
        IPiece Transpose();
        bool TryUpdateBoard(ref IBoard board, int position, char code);
        int Width { get; }
    }
}