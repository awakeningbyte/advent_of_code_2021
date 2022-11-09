open System
let inputs=IO.File.ReadAllLines("input.txt") 

let crabs = inputs.[0].Split(",") |> Array.map int |> Array.sort
// crabs
//     |> Seq.iter(fun i-> (printfn "   %i" i))

let n = crabs.[0]

// printfn "%i %i %i" n (Array.last crabs) crabs.Length

let dist (x: int) (p:int) = 
        let d = Math.Abs(p - x)
        d * (d+1)/2
let init = crabs 
        |> Array.map(fun i -> dist n i)
        |> Array.sum
let Cal (p, fuel) n =
        let cs = crabs 
                |> Array.map(fun i -> dist n i)
                |> Array.sum
        if fuel > cs then
                (n, cs)
        else 
                (p, fuel)

let (p2, p2_fule) = [n..(Array.last crabs)] |> Seq.fold Cal (n, init)

// printfn "%i %i %i" n (Array.last crabs) init 
printfn "p2: %i %i" p2 p2_fule