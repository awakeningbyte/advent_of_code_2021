open System

let inputs = IO.File.ReadAllLines("input.txt") |> Array.toList
// printfn "%s" inputs.[0]
let (draws_array, boards) = 
    match inputs with
    | head::tail-> 
                        let d =head.Split(",") 
                                        |> Array.map int
                        (d, tail)
    | []-> failwith "input file is empty"

let draws_set = Set.ofArray draws_array

let draws_map = seq {for i in 0..(draws_array.Length-1) do (draws_array.[i],i)} |> Map.ofSeq

let (board: int[] list) = []
let cal_score (acc, limit) (r: int[]) =
        let s =
            r 
            |> Seq.filter (fun i ->
                    ( not (draws_map.ContainsKey(i)) || draws_map.[i] > limit))
            |> Seq.sum

        (acc+s, limit)

let cal (board: int[] list, best_score: int, best_dist: int) =
    match board with
    | [] -> (([]: int[] list),best_score, best_dist) // init with empty board 
    | [r1 ;r2 ;r3;r4;r5] ->
            let rows =  [r1; r2; r3; r4;  r5]
            let cols = [for i in 0..4 do [|r1.[i];r2.[i]; r3.[i]; r4.[i]; r5.[i]|] ]

            let completed = 
                rows @ cols
                |> Seq.map Set.ofSeq
                |> Seq.filter(fun r -> Set.isSubset r draws_set)

            let (last_call, last_idx) =
                completed
                |> Seq.map(fun s ->
                    s 
                    |> Seq.map (fun i -> (i, draws_map.[i]))
                    |> Seq.maxBy(fun (i, idx) -> idx))
                |> Seq.minBy(fun (i, idx) -> idx)
            
            if last_idx < best_dist then
                (([]: int[] list),best_score, best_dist) 
            else 
                let (new_score, _) = 
                    [r1; r2; r3; r4;  r5]
                    |> Seq.fold cal_score (0, last_idx)

                (([]: int[] list),new_score * last_call, last_idx) 
    | _ -> failwith "invalid board"

let play (board: int[] list, best_score: int, best_dist: int) (row: int[] option)=
    match row with
    | None -> cal (board, best_score, best_dist) //complete reading in a board
    | Some(r) -> (board @ [r], best_score, best_dist) //constructing a board

let (_, score, _) = 
    boards 
    |> Seq.map(fun s-> 
        match s with
        | "" -> None
        | _ -> 
            let row =s.Split(" ") |> Array.filter(fun (i) -> not ( String.IsNullOrEmpty(i))) |> Array.map int 
            //printfn ("%s %i") s row.Length
            Some(row))
    |> Seq.fold play (board,0, 0)

printfn "%i" score