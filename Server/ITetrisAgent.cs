namespace Tim.Tetris.Server
{
    public interface ITetrisAgent
    {
        TetrisMove GetNextMove(string piece, string board);
    }
}