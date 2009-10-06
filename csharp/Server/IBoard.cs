namespace Tim.Tetris.Server
{
    public interface IBoard
    {
        IBoard Collapse(out int score);
        IBoard Update(int row, string code);
        string this[int row] { get; }
        int Height { get; }
    }
}