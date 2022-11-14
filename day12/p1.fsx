open System
open System.Collections.Generic
let inputs = IO.File.ReadAllLines("test.txt")

let graph =
  inputs 
  |> Seq.map (fun line -> 
    match line.Split("-") with
    | [|"start"; b|] -> [| ("start", b)|]
    | [|a; "end"|] -> [| (a, "end") |]
    | [|a; b|] -> [| (a, b); (b,  a) |]
    | _ -> failwith "invalid inputs"
    )
  |> Seq.collect(fun a -> a)
  |> Seq.groupBy(fun (a,b) -> a)
  |> Map.ofSeq

let visited  =
  graph
  |> Seq.map(fun p -> (p.Key, (false, Seq.empty<string>)))
  |> dict
  |> Dictionary

visited.Add("end", (false, Seq.empty<string>))

let rec bfs que =
  match que with
  |[]  -> ()
  | "end" :: tail -> bfs tail
  | h::tail ->
    // printf "deque %s\n" h
    let toAdd =
      graph[h] //linked nodes
      |> Seq.filter(fun (_, linked) -> // remove visited
          let (b, _) = visited.[linked]
          not b
      ) //filter all visited
      |> Seq.map(fun (_, linked) ->
        let (_, tr) = visited.[linked]
        // let trace = tr
        let trace1 = seq { yield! tr; h } |> Array.ofSeq
        
        match linked with
        | "end" -> 
          visited.[linked] <- (false, Set.ofSeq trace1) 
          // printfn "%s -> %s %A " h "end" visited.[linked]
        | x when ("A" <= x) && ("Z" >= x) -> 
          visited.[linked] <- (false, Set.ofSeq  trace1) 
          // printfn "%s -> %s %A " h x visited.[linked]
        | z when ("a" <= z) && ("z" >= z) -> 
          visited.[linked] <- (true, Set.ofSeq  trace1) 
          // printfn "%s -> %s %A " h z visited.[linked]
        | _ -> failwith "invalid input"
        |> ignore

        linked
      )
 
    bfs [ yield! tail ; yield! toAdd ]

bfs ["start"]

let rec BackTrace node (upstream: list<string>) =
  match node with
  | "start" -> seq { ["start"]}
  | _ ->
    let (_, trace) = visited.[node]
    trace
    |> Seq.collect(fun x -> 
        match x with
        | _ when ("a" <= x) && ("z" >= x) && (List.exists(fun i -> i =x) upstream)->  Seq.empty
        | _ ->
          BackTrace x [yield! upstream; x]
          |> Seq.map(fun y -> 
            [ yield! y; node ]
          )
      )
 
// printfn "%A" visited.["end"]
BackTrace "end" []
|> Seq.iter (fun x -> 
    printfn "%A" x
  )