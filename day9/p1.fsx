open System
let inputs = IO.File.ReadAllLines("input.txt")

type Point = {n: char; low: bool}
let grid =
  inputs
  |> Array.map (fun i->
      i.ToCharArray()
      |> Array.map (fun c->
            {n = c; low = true}
        ))
let size = grid.[0].Length
let horizontaled = 
  grid
    |> Array.map (fun line->
        let paddedX =[|{n='A'; low = false}; yield! line; { n = 'A'; low = true}|]
        paddedX
        |> Array.windowed(3)
        |> Array.map (fun w ->
            match w with
            | [|l;m;r|] -> 
              {n = m.n; low = (l.n > m.n && r.n > m.n)}
            | _ -> failwith "invalid input"
          ))

let paddedY = 
  [| 
      [|for _ in 1..size do {n = 'A'; low = false}|] 
      yield! horizontaled
      [|for _ in 1..size do {n = 'A'; low = false}|] 
  |]
  |> Array.windowed(3)
  |> Array.map (fun lines ->
      match lines with
      | [|t; m; b;|] ->
        // m |> Array.map(f)    printfn "%c %b;" m.n (l.n > m.n && r.n > m.n)
        Array.map3 (fun tv mb bn -> {n = mb.n; low = (mb.low && (tv.n > mb.n) && (bn.n > mb.n))}) t m b
      | _ -> failwith "invalid inputs"
    )

paddedY
|> Array.map(fun (line: Point[]) -> 
    line
    |> Array.filter( fun i -> i.low )
    |> Array.sumBy( fun i -> 
      // printfn "- y: %c" i.n
      (int i.n) - (int '0') + 1)
    )
|> Array.sum
|> printfn "%i"