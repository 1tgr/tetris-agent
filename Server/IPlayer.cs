namespace Tim.Tetris.Server
{
    public interface IPlayer
    {
        TetrisMove MovePiece(IBoard board, IPiece piece);
    }
}