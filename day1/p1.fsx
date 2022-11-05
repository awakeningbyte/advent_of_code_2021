open System

let inputs = IO.File.ReadAllLines("input.txt")

let p1 = inputs
            |> Array.map int
            |> Array.pairwise
            |> Array.map (fun (a,b) -> a < b)
            |> Array.countBy (fun a->a)
let _ ,c = p1[0]
printfn "part 1 answer: %i" c


let p2 = inputs
        |> Array.map int
        |> Array.windowed(3)
        |> Array.map(Array.sum)
        |> Array.pairwise
        |> Array.map (fun (a,b) -> a < b)
        |> Array.countBy (fun a->a)

let _ ,d = p2[0]
printfn "part 2 answer: %i" d
