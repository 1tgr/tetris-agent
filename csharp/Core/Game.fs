#light
namespace Tim.Tetris.Core
open System

module Game =
    type Board =
        {
            BoardData : Map<int, string>
        }

    type Piece =
        {
            Width : int;
            Height : int;
            PieceData : Set<int * int>
        }

    let empty =
        {
            BoardData =
                [
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    "..........";
                    ".........."
                ]
                |> List.mapi (fun index item -> (index, item))
                |> Map.of_list
        }

    let parse (s : string) =
        {
            BoardData =
                s.Split([| ' '; '\n' |])
                |> List.of_array
                |> List.mapi (fun row boardCode -> row, boardCode.Trim())
                |> Map.of_list
        }

    let width = 10
    let degreesOptions = [ 0; 90; 180; 270 ]

    let transposeArray x =
        let result = Array2D.zeroCreate (Array2D.length1 x) (Array2D.length2 x)
        Array2D.iteri (fun i j value -> result.[j, i] <- value) x
        result

    let rec rotate pieceData degrees =
        match degrees with
        | 0 -> pieceData
        | 90 -> rotate (transposeArray pieceData) 0
        | 180 -> rotate (transposeArray pieceData) 90
        | 270 -> rotate (transposeArray pieceData) 180
        | _ -> raise <| new ArgumentOutOfRangeException("degrees")

    let isCollision { PieceData = pieceData } { BoardData = boardData } row column =
        let isCollision' (pieceRow, pieceColumn) =
            let boardCode = boardData.[row + pieceRow]
            boardCode.[column + pieceColumn] <> '.'

        Set.exists isCollision' pieceData

    let draw { PieceData = pieceData } board row column code =
        let draw' ({ BoardData = boardData } as board) (pieceRow, pieceColumn) =
            let boardCode = boardData.[row + pieceRow]
            let newBoardCode = boardCode.Substring(0, column + pieceColumn) + code + boardCode.Substring(column + pieceColumn + 1)
            { board with BoardData = boardData |> Map.remove (row + pieceRow) |> Map.add (row + pieceRow) newBoardCode }

        Set.fold draw' board pieceData

    let tryUpdateBoard piece ({ BoardData = boardData } as board) column code =
        let rows = 
            boardData
            |> Map.to_seq
            |> Seq.sortBy (fst >> (-))

        match Seq.tryFindIndex (fun (row, _) -> isCollision piece board row column) rows with
        | Some row -> (true, Some <| draw piece board row column code)
        | None -> (false, None)

    let collapse board =
        let rec collapse' ({ BoardData = boardData } as board) row firstRow collapsedRows score =
            let collapse'' ({ BoardData = boardData } as board) firstRow =
                function
                | 0 -> board
                | rows ->
                    let newBoardData =
                        boardData
                        |> Map.to_seq
                        |> Seq.map (fun (row, boardCode) ->
                            if row < rows then
                                row, ".........."
                            else if row < firstRow then
                                row, boardData.[row + 1]
                            else
                                row, boardCode)
                        |> Map.of_seq

                    { board with BoardData = newBoardData }

            match Map.tryFind row boardData with
            | Some boardCode ->
                if boardCode.Contains(".") then
                    collapse' (collapse'' board firstRow collapsedRows) (row + 1) 0 0 (score + collapsedRows)
                else if collapsedRows = 0 then
                    collapse' board (row + 1) row (collapsedRows + 1) score
                else
                    collapse' board (row + 1) firstRow (collapsedRows + 1) score

            | None -> 
                (score + collapsedRows, collapse'' board firstRow collapsedRows)

        collapse' board 0 0 0 0

    let randomPlayer (random : Random) =
        let getDepths board =
        {
            int[] heights = new int[Board.Width];

            for (int column = 0; column < Board.Width; column++)
                heights[column] = board.Height + 1;

            for (int row = board.Height - 1; row >= 0; row--)
            {
                for (int column = 0; column < Board.Width; column++)
                {
                    if (board[row][column] != '.')
                        heights[column] = row + 1;
                }
            }

            return heights;
        }

        let findDeepestColumn board pieceWidth =
            let depths = getDepths board

            int deepestColumn = 0;
            int deepestOuter = 0;
            for (int column = 0; column <= Board.Width - pieceWidth; column++)
            {
                int shallowestInner = board.Height + 1;
                for (int pieceColumn = 0; pieceColumn < pieceWidth; pieceColumn++)
                {
                    if (depths[column + pieceColumn] < shallowestInner)
                        shallowestInner = depths[column + pieceColumn];
                }

                if (shallowestInner > deepestOuter)
                {
                    deepestOuter = shallowestInner;
                    deepestColumn = column;
                }
            }

            return deepestColumn;
        }
        
        let movePiece board piece =
            let rand = random.Next(Array.length degreesOptions)
            let degrees = degreesOptions.[rand]
            let { Width = width } = rotate PieceData.All[piece] degrees
            let position = findDeepestColumn board width
            TetrisMove (position, degrees)

        movePiece
