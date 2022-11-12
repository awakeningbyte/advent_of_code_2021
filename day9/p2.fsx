open System
open System.Collections.Generic
let inputs = IO.File.ReadAllLines("input.txt")

let sizeX = inputs.[0].Length
let sizeY = inputs.Length

type Position = 
  {x: int; y:int;}
  with static member Default = {x = -1 ; y = -1 }

type Point =
  {n: char; low: bool; position: Position}
  with static member Padding = { n = 'A'; low = false; position = Position.Default }

type Link =
  { incomings: Set<Position>; towards: Option<Position> }
  with static member Empty = { incomings =  Set.empty; towards= None }

let mem = new Dictionary<Position,Link>() 

//build init grid
let grid =
  [0..(inputs.Length - 1)]
  |> Seq.map(fun i ->
    let line = inputs.[i]
    let numbers = line.ToCharArray()
    [0..numbers.Length - 1] 
    |> List.map(fun x ->
      let p = {x = x; y = i}
      mem.Add(p, Link.Empty)
      { n = numbers[x]; low = true; position = p}
    ))

let makeLink o m =
  match o with
    | {n = '9'; low = _ ; position = _ } | {n = 'A' ; low = _; position = _ } ->  ()
    | {n = _; low = _; position = p} when o.n > m.n->
        let linkO = mem.[o.position]
        mem.[o.position] <- {linkO with towards = Some(m.position)}
        let linkM = mem.[m.position]
        mem.[m.position] <- {linkM with incomings = linkM.incomings.Add(o.position)}
    | {n = _; low = _; position = p} when o.n < m.n-> 
        let linkO = mem.[o.position]
        mem.[o.position] <- {linkO with incomings = linkO.incomings.Add(m.position)}
        let linkM = mem.[m.position]
        mem.[m.position] <- {linkM with towards = Some(o.position)}
    | _ -> ()
        
let takeLine line = 
  let paddedX  = seq { Point.Padding; yield! line; Point.Padding }
  paddedX
  |> Seq.windowed(3)
  |> Seq.map (fun w ->
      match w with
      | [|l;m;r|] -> 
        let isLow = 
          match m with
          | {n = '9'; low = _ ; position = _ } -> false
          | {n = 'A' ; low = _; position = _ } -> false
          | {n = value; low = false; position = p} -> failwith "initial value should be true"
          | {n = value; low = _ ; position = p} ->
            makeLink l m
            makeLink r m
            l.n > m.n && r.n > m.n

        { m with low = isLow}
      | _ -> failwith "invalid input"
    )

//horizontally process
let horizontaled = 
  grid |> Seq.map  takeLine

//vertiallly process
let paddedY = 
  [| 
      seq { for _ in 1..sizeX do Point.Padding }
      yield! horizontaled
      seq { for _ in 1..sizeX do Point.Padding } 
  |]
  |> Array.windowed(3)
  |> Array.map (fun lines ->
      match lines with
      | [|t; m; b;|] ->
        // m |> Array.map(f)    printfn "%c %b;" m.n (l.n > m.n && r.n > m.n)
        Seq.map3 (fun tv mb bn -> {mb with low = (mb.low && (tv.n > mb.n) && (bn.n > mb.n))}) t m b
      | _ -> failwith "invalid inputs"
    )

paddedY
|> Seq.map(fun (line: seq<Point>) -> 
    line
    |> Seq.filter( fun i -> i.low )
    |> Seq.sumBy( fun i -> 
      // printfn "- y: %c" i.n
      (int i.n) - (int '0') + 1)
    )
|> Seq.sum
|> printfn "%i"
