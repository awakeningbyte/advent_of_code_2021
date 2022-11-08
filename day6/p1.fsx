open System
let inputs=IO.File.ReadAllLines("input.txt")

let herd = inputs.[0].Split(',') |> Array.map int |> Seq.ofArray

let move (herd: seq<int>) (i: int) =
    herd 
    |> Seq.collect(fun h-> 
        match h with
        | 0 -> seq {6;8}
        | x -> seq {(x-1)}
    )

let p1 =[1..80]
        |> List.fold move herd
        |> Seq.length


printfn "part 1: %i" p1