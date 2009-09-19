namespace Tim.Tetris.Server
{
    public interface IPlayer
    {
        TetrisMove MovePiece(IBoard board, Piece piece);
    }
}