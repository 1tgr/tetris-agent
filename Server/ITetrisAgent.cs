namespace Tim.Tetris.Server
{
    public interface ITetrisAgent
    {
        TetrisMove MovePiece(string board, IPiece piece);
    }
}