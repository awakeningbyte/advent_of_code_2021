open System
let inputs=IO.File.ReadAllLines("test.txt")

let (|Position|_|) (str: string) =
    match str.Split(",") with
    | [|x;y|] ->  Some(int x, int y)
    | _ -> None

let (|FoldX|_|) (str: string) =
    match str.Split([|' '; '='|]) with
    | [|"fold"; "along"; "x"; n|] ->  
        let f = int n
        let fx = fun (x: int ,y: int) ->
            if x = f then
                None
            else
                (Some(x, (2*f - x)))
        Some(fx)
    | _ -> None

let (|FoldY|_|) (str: string) =
    match str.Split([|' '; '='|]) with
    | [|"fold"; "along"; "y"; n|] ->  
        let f = int n
        let fx = fun (x: int ,y: int) ->
            if y = f then
                None
            else
                (Some((x, (2*f - y))))
        Some(fx)
    | _ -> None

inputs
|> Seq.fold(fun (points: list<(int * int)> , folds) line -> 
    match line with
    | Position p -> ([p; yield! points], folds)
    | FoldX p -> (points, [p; yield! folds])
    | FoldY p -> (points, [p; yield! folds])
    | _ -> (points, folds)
) ([], [])
|> fun (points, folds) ->
    folds
    |> Seq.fold(fun acc op ->
        acc
        |>  Seq.map op
        |> Seq.filter(fun p -> p.IsSome)
        |> Seq.map(fun p -> p.Value)
    ) points
|> Set.ofSeq
|> Seq.length
|> printfn "%i"