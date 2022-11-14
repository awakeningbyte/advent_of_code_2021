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

let rec bfs que =
  match que with
  |[] -> ()
  | h::tail ->

    let toAdd =
      graph[h] //linked nodes
      |> Seq.filter(fun (_, linked) -> // remove visited
          let (b, _) = visited.[linked]
          not b
      ) //filter all visited
      |> Seq.map(fun (_, linked) ->
        let (_, tr) = visited.[linked]
        // let trace = tr
        let trace1 = seq { yield! tr; h }
        let mark =
          match h with
          | "start" | "end" -> true
          | x when ("A" <= x) && ("Z" >= x) -> false
          | z when ("a" <= z) && ("z" >= z) -> true
          | _ -> failwith "invalid input"
        visited.[linked] <- (mark, trace1) //mark visit
        linked
      )
 
    bfs [ yield! tail ; yield! toAdd ]

bfs ["start"]