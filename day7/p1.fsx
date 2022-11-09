open System
let inputs=IO.File.ReadAllLines("input.txt") 

let crabs = inputs.[0].Split(",") |> Array.map int |> Array.sortDescending

let n = (Array.length crabs) /2
let c = crabs.[n]
let p1 =crabs 
        |> Array.map(fun i -> i - c)
        |> Array.map Math.Abs
        |> Array.sum

printfn "%i" p1

