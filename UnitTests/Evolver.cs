using System;
using System.Collections.Generic;
using NUnit.Framework;
using Tim.Tetris.Server;

namespace Tim.Tetris.UnitTests
{
    [TestFixture]
    public class Evolver
    {
        private class Individual
        {
            private readonly double[,] positionMatrix;
            private readonly double[,] rotationMatrix;

            private Individual(double[,] positionMatrix, double[,] rotationMatrix)
            {
                this.positionMatrix = positionMatrix;
                this.rotationMatrix = rotationMatrix;
            }

            private static double[,] CreateRandomMatrix(Random random, int rows, int columns)
            {
                double[,] matrix = new double[rows,columns];

                for (int row = 0; row < rows; row++)
                {
                    for (int column = 0; column < columns; column++)
                        matrix[row, column] = (random.NextDouble() * 2) - 1;
                }

                return matrix;
            }

            public static Individual CreateRandom(Random random)
            {
                return new Individual(CreateRandomMatrix(random, 3, 10), CreateRandomMatrix(random, 3, 10));
            }

            public double[,] PositionMatrix
            {
                get { return positionMatrix; }
            }

            public double[,] RotationMatrix
            {
                get { return rotationMatrix; }
            }
        }

        private static readonly Piece[] Pieces = (Piece[])Enum.GetValues(typeof(Piece));

        private static TetrisMove MovePiece(IBoard board, Piece pieceCode, Individual individual)
        {
            int[] depths = Board.GetDepths(board);
            double position = 0, rotation = 0;

            for (int column = 0; column < depths.Length; column++)
            {
                for (int term = 0; term < 2; term++)
                {
                    double pow = Math.Pow(20 - depths[column], term);
                    position += individual.PositionMatrix[term, column] * pow;
                    rotation += individual.RotationMatrix[term, column] * pow;
                }
            }

            int normalizedRotation = ((int)Math.Abs(rotation)) % Board.DegreesOptions.Length;
            int[,] pieceData = PieceData.All[pieceCode];
            int[,] rotatedPieceData = Board.Rotate(pieceData, Board.DegreesOptions[normalizedRotation]);
            int normalizedPosition = ((int)Math.Abs(position)) % (10 - rotatedPieceData.GetLength(1));
            return new TetrisMove(normalizedPosition, Board.DegreesOptions[normalizedRotation]);
        }

        private static int Play(Random random, Individual individual)
        {
            IBoard board = Board.Empty;
            Piece pieceCode;
            int[,] rotatedPieceData;
            TetrisMove move;
            int totalScore = 0;

            do
            {
                int score;
                board = board.Collapse(out score);
                totalScore += score;

                pieceCode = Pieces[random.Next(Pieces.Length)];

                try
                {
                    move = MovePiece(board, pieceCode, individual);
                    CollectionAssert.Contains(Board.DegreesOptions, move.Degrees, "Degrees");
                    Assert.GreaterOrEqual(move.Position, 0, "Position");

                    int[,] pieceData = PieceData.All[pieceCode];
                    rotatedPieceData = Board.Rotate(pieceData, move.Degrees);
                    Assert.LessOrEqual(move.Position, 10 - rotatedPieceData.GetLength(1), "Position");
                }
                catch (Exception ex)
                {
                    throw new AssertionException(ex.Message + " Piece is " + pieceCode + ".", ex);
                }
            } while (Board.TryUpdateBoard(rotatedPieceData, ref board, move.Position, char.ToLower(pieceCode.ToString()[0])));

            return totalScore;
        }

        [Test]
        public void Evolve()
        {
            Random random = new Random(0);
            Dictionary<Individual, int> population = new Dictionary<Individual, int>();
            int attempts = 0;

            while (population.Count < 10)
            {
                Individual individual = Individual.CreateRandom(random);
                int score = Play(random, individual);
                if (score > 0)
                    population.Add(individual, score);

                attempts++;
                if ((attempts % 1000) == 0)
                    Console.WriteLine("{0} attempts", attempts);
            }
        }
    }
}