open System
let inputs = IO.File.ReadAllLines("input.txt")

let sizeX = inputs.[0].Length
let sizeY = inputs.Length

type Point = 
  { X:int; Y: int; E: int}

let grid = 
  [|0..sizeY-1|]
  |> Array.map (fun i->
    let line = inputs[i].ToCharArray()
    [|0..sizeX-1|]
    |> Array.map (fun j->
      {X = j; Y = i; E = (int line[j]- int '0')}
      )
  )

let run (g: Point[][]) =
  let grid =
    g
    |> Array.map (fun line->
        line
        |> Array.map (fun p ->
          let e1 =
            match p.E with
            | 0 -> 1
            | 9 -> 0
            | n -> n + 1
          {p with E = e1})
      )
  
  let flash =
    grid
    |> Seq.map (fun g ->
        g
        |> Seq.filter(fun p -> p.E = 0)
      )
    |> Seq.collect(fun g -> g)
    |> List.ofSeq

  (grid, flash)

let neighbors (p: Point) =
  [1;-1;0]
  |> Seq.map (fun x ->
    [1;-1;0]
    |> Seq.map (fun y -> {E = -1;  X = p.X + x; Y = p.Y + y} )
    |> Seq.filter (fun n -> 
      n.X >= 0 &&
      n.Y >= 0 &&
      n.X < sizeX &&
      n.Y < sizeX &&
      (n.X <> p.X || n.Y <>p.Y)))
  |> Seq.collect(fun i -> i)
  |> Seq.toArray


// printfn " %A"  ns
let rec spread (g: Point[][], ns) =
  let flashs =
    ns
    |> Seq.collect(fun n -> neighbors n)
    |> Seq.fold (fun acc n ->
      let target = g[n.Y][n.X]
      match target.E with
      | 0 -> 
        // g[n.Y][n.X] <- {target with E = 1}
        acc
      | 9 ->
        g[n.Y][n.X] <- {target with E = 0}
        [yield! acc; n]
      | v -> 
        g[n.Y][n.X] <- {target with E = v+ 1}
        acc
    ) []
  
  match flashs with
  | [] -> g
  | _ -> spread (g, flashs)
  
// let (g, f) = run grid 
[1..1000] 
|> Seq.fold (
    fun (acc,c) r -> 
      let g1 = run acc |> spread 
      let c1 =
        g1
        |> Seq.map ( fun g ->  g |> Seq.filter (fun i -> i.E = 0)|> Seq.length)
        |> Seq.sum
      if (c1) = sizeX * sizeY then
        printfn "p2: %i: %i" r (c1)
      (g1, c+c1)
    ) (grid, 0)
|> fun (_, c) -> printfn "%i" c