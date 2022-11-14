open System
let inputs = IO.File.ReadAllLines("test.txt")

let graph =
  inputs 
  |> Seq.map (fun line -> 
    match line.Split("-") with
    | [|"start"; b|] -> [| [|"start"; b|]|]
    | [|a; "end"|] -> [| [|a; "end"|] |]
    | [|a; b|] -> [| [|a; b|]; [|b;  a|] |]
    | _ -> failwith "invalid inputs"
    )
  |> Seq.collect(fun a -> a)
  |> Seq.groupBy(fun a ->
    match a with
    | [|x;y|] -> x
    | _ -> failwith "invalid input"
  )
  |> Map.ofSeq