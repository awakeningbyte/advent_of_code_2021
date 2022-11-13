open System
open System.Collections.Generic
let inputs = IO.File.ReadAllLines("input.txt")

let sizeX = inputs.[0].Length

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
  |> List.map(fun i ->
    let line = inputs.[i]
    let numbers = line.ToCharArray()
    [0..numbers.Length - 1] 
    |> List.map(fun x ->
      let p = {x = x; y = i}
      mem.Add(p, Link.Empty)
      { n = numbers[x]; low = true; position = p}
    ))

let makeLink2 o m =
  match o with
    | {n = '9'; low = _ ; position = _ } -> ()
    | {n = 'A' ; low = _; position = _ } -> ()
    | {n = _; low = _; position = p} when o.n > m.n ->
        let linkM = mem.[m.position]
        match linkM.towards with
        | Some(p) -> ()
        | None ->
          let linkO = mem.[o.position]
          mem.[o.position] <- {linkO with towards = Some(m.position)}
          mem.[m.position] <- {linkM with incomings = linkM.incomings.Add(o.position)}
    | {n = _; low = _; position = p} when o.n < m.n-> 
        let linkO = mem.[o.position]
        let linkM = mem.[m.position]
        match linkM.towards with
        | Some(p) -> 
          ()
        | None ->
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
            makeLink2 l m
            makeLink2 r m
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
        Seq.map3 (fun tv mb bn -> 
          let isLow = 
            match mb with
            | {n = '9'; low = _ ; position = _ } -> false
            | {n = 'A' ; low = _; position = _ } -> false
            | {n = value; low = _ ; position = p} ->
              makeLink2 tv mb
              makeLink2 bn mb
              
              (mb.low && (tv.n > mb.n) && (bn.n > mb.n))

          {mb with low = isLow }) t m b

      | _ -> failwith "invalid inputs"
    )
|> Array.map Array.ofSeq

let rec collectPostions (p: Position) =
  let incomingsSet = mem.[p].incomings
  
  let sets = 
    match incomingsSet with
    | _ when incomingsSet.IsEmpty -> seq {p}
    | incomings ->  
      seq { p; yield! incomings |> Seq.collect(fun (i: Position) -> collectPostions i) }
  sets

let a = 
    Array.ofSeq paddedY
    |> Array.collect(fun line -> line)
    |> Seq.filter( fun i -> i.low )
    |> Array.ofSeq
    |> Array.map (fun l ->
        // printfn "!%c" l.n
        (collectPostions l.position) 
        // |> Seq.map(fun c -> 
        //   printfn "%c: (y:%i,x%i) %A" grid.[c.y].[c.x].n c.y c.x mem.[c]
        //   c
        // )
        |> Set.ofSeq
        |> Seq.length
        )
      // |> Seq.sum)
      |> Seq.sortDescending
  
a |> Seq.take(3) |> Seq.reduce(fun acc i -> acc * i) |> (printfn "%i")