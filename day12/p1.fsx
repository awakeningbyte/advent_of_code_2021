open System
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

let rec bfs que =
  match que with
  |[] -> ()
  | h::remains ->
    let links = graph[h]
 
    bfs remains

bfs ["start"]