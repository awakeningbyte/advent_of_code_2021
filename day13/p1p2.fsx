open System
let inputs=IO.File.ReadAllLines("input.txt")


let (|Position|_|) (str: string) =
    match str.Split(",") with
    | [|x;y|] ->  Some(int x, int y)
    | _ -> None

let (|FoldX|_|) (str: string) =
    match str.Split([|' '; '='|]) with
    | [|"fold"; "along"; "x"; n|] ->  
        let f = int n
        let fx = fun (x: int ,y: int) ->
            match x with
            | s when x > f -> (Some(((2*f - x), y)))
            | s when x < f -> (Some((x, y)))
            | _ -> None
        Some(fx)
    | _ -> None

let (|FoldY|_|) (str: string) =
    match str.Split([|' '; '='|]) with
    | [|"fold"; "along"; "y"; n|] ->  
        let f = int n
        let fx = fun (x: int ,y: int) ->
            // printfn "---f:%i (%i, %i)" f x y 
            match y with
            | s when y > f -> (Some((x, (2*f - y))))
            | s when y < f -> (Some((x, y)))
            | _ -> None
        Some(fx)
    | _ -> None

inputs
|> Seq.fold(fun (points: list<(int * int)> , folds) line -> 
    // printfn "\t%s" line
    match line with
    | Position p -> ([p; yield! points], folds)
    | FoldX p -> (points, [yield! folds;p])
    | FoldY p -> (points, [yield! folds;p])
    | _ -> (points, folds)
) ([], [])
|> fun (points, folds) ->
    // printfn "folds: %i %A" folds.Length folds[0]
    folds
    |> Seq.fold(fun acc op ->
        acc
        |> Seq.map op
        |> Seq.filter(fun p -> p.IsSome)
        |> Seq.map(fun p -> p.Value)
    ) points
|> Set.ofSeq
|> Seq.groupBy(fun (x, y)-> y)
|> Seq.sortBy( fun (k, v) -> k)
|> Seq.fold (fun c (k, v) -> 
    let s = [|for i in [1..40] do ' ' |]
    if c < k then
        [1..(k-c)]
        |> Seq.iter(fun _ -> printfn "                 ")
    
    v
    |> Seq.iter (fun (x,y) -> s[x] <- '#')
    System.String s |> printfn "%s"

    k+1
) 0