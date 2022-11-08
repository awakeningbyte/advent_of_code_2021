open System
let inputs=IO.File.ReadAllLines("input.txt")

inputs 
|> Array.map(fun s -> s.Split([|"->"; ","|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int)
    // lines
|> Array.map(fun l ->
        match l with
        | [|a;b; c; d|] when (a = c) && (b <= d)-> [|for i in b..d do (a, i)|]
        | [|a;b; c; d|] when (b = d) && (a <= c)-> [|for i in a..c do (i, b)|]
        | [|a;b; c; d|] when (a = c) -> [|for i in d..b do (a, i)|]
        | [|a;b; c; d|] when (b = d) -> [|for i in c..a do (i, b)|]
        | _ -> [||])
// |> Array.iter(fun i -> i  |> Array.iter(fun (x,y)-> printfn"%i:%i " x y))
|> Array.collect( fun a -> a)
|> Array.groupBy(fun a -> a) 
|> Array.filter(fun (key, p) -> p.Length > 1)   
|> Array.length
|> printfn ("%i")

