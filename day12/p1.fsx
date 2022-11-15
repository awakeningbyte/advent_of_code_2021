open System
open System.Collections.Generic
let inputs = IO.File.ReadAllLines("input.txt")

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

let rec explore (node: string) (upstream: seq<string>) (mem: Map<string, bool>) =
  let upstream1 = seq {yield! upstream; node }
  match node with
  | "end" -> seq { upstream1 }
  | _ when mem.ContainsKey node -> seq {upstream}
  | n when ("A" <= n && "Z" >= n) -> 
      graph[node]
        |> Seq.map(fun (_, n) ->  n )
        |> Seq.collect(fun n -> explore n upstream1 mem)
  | _ ->
    let mem1 =mem.Add(node,true)
    graph[node]
    |> Seq.map (fun (_, n) -> n )
    |> Seq.collect (fun n -> explore n upstream1 mem1)

explore "start" [] Map.empty<string,bool>
|> Seq.filter(fun l -> (Seq.last l) = "end")
|> Seq.length
|> printfn "%i"
// |> Seq.iter(fun i -> printfn "%A" i)