using System;
using System.Collections.Generic;
using System.Linq;
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

                for (int row = 0; row < matrix.GetLength(0); row++)
                {
                    for (int column = 0; column < matrix.GetLength(1); column++)
                        matrix[row, column] = (random.NextDouble() * 2) - 1;
                }

                return matrix;
            }

            private static double[,] MutateMatrix(Random random, double[,] other, double amount)
            {
                double[,] matrix = new double[other.GetLength(0), other.GetLength(1)];

                for (int row = 0; row < matrix.GetLength(0); row++)
                {
                    for (int column = 0; column < matrix.GetLength(1); column++)
                        matrix[row, column] = other[row, column] + ((random.NextDouble() * 2) - 1) * amount;
                }

                return matrix;
            }

            public static Individual CreateRandom(Random random)
            {
                return new Individual(CreateRandomMatrix(random, 3, 10), CreateRandomMatrix(random, 3, 10));
            }

            public Individual Mutate(Random random)
            {
                return new Individual(MutateMatrix(random, positionMatrix, 0.1), MutateMatrix(random, rotationMatrix, 0.1));
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

        private static double Play(Individual individual)
        {
            Random random = new Random(0);
            int totalScore = 1;
            int turns = 0;

            for (int game = 0; game < 100; game++)
            {
                IBoard board = Board.Empty;
                Piece pieceCode;
                int[,] rotatedPieceData;
                TetrisMove move;

                do
                {
                    int score;
                    board = board.Collapse(out score);
                    totalScore += score;
                    turns++;

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
            }

            return ((double) totalScore) / turns;
        }

        [Test]
        public void Evolve()
        {
            Random random = new Random(0);
            Dictionary<Individual, double> population = new Dictionary<Individual, double>();

            while (population.Count < 100)
            {
                Individual individual = Individual.CreateRandom(random);
                double score = Play(individual);
                population.Add(individual, score);
            }

            double? previousBest = null;

            for (int generation = 0; generation < 1000; generation++)
            {
                KeyValuePair<Individual, double> fittest =
                    population
                        .OrderByDescending(p => p.Value)
                        .First();

                if (previousBest == null || fittest.Value > previousBest)
                {
                    previousBest = fittest.Value;

                    Console.WriteLine(
                        "Generation {0}: best {1}, average {2}",
                        generation,
                        fittest.Value,
                        population.Values.Average());
                }

                population.Clear();
                population.Add(fittest.Key, fittest.Value);

                while (population.Count < 100)
                {
                    Individual individual = fittest.Key.Mutate(random);
                    double score = Play(individual);
                    population.Add(individual, score);
                }
            }
        }
    }
}