open System
open System.Collections.Generic

let inputs=IO.File.ReadAllLines("input.txt")

let unique (patterns: string[]) = 
    patterns
    |> Array.map(fun p -> 
        match p.Length with
        | 2 | 3 |4 |7 -> 1
        | _ -> 0)
    |> Array.sum

let analyze ((patterns:Set<char>[]),(outputs:Set<char>[])) =
    let m = new Dictionary<int,list<char>>()
    let x = (Set.difference patterns[1]  patterns[0]) |> Set.toArray
    m.Add(1, [x.[0]])
    //m

let setEach (a:string[]) =
    a
    |> Array.map(fun i -> i.ToCharArray() |> Set.ofArray)

inputs
|> Array.map(fun line -> line.Split(" | "))
|> Array.map(fun parts ->
    match parts with
    | [|signals;outputs|] -> (signals.Trim().Split(" ") |> setEach, outputs.Trim().Split(" ") |> setEach)
    | _ -> failwith "invalid input")
|> Array.map analyze