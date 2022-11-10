open System
let inputs=IO.File.ReadAllLines("input.txt")

let unique (patterns: string[]) = 
    patterns
    |> Array.map(fun p -> 
        match p.Length with
        | 2 | 3 |4 |7 -> 1
        | _ -> 0)
    |> Array.sum

inputs
|> Array.map(fun line -> line.Split(" | "))
|> Array.map(fun parts ->
    match parts with
    | [|signals;outputs|] -> (signals.Trim().Split(" "), outputs.Trim().Split(" "))
    | _ -> failwith "invalid input")
|> Array.map(fun (_, outputs) ->
    unique(outputs)
)
|> Array.sum
|> printfn "%i"