open System
open System.Collections.Generic

let inputs=IO.File.ReadAllLines("input.txt")

let analyze ((patterns:list<char>[]),(outputs:list<char>[])) =
    let m: Dictionary<int, Set<char>> = new Dictionary<int,Set<char>>()
    let x = 
        patterns 
        |> Seq.sortBy(fun p ->p.Length) 
        |> Seq.map(fun i -> Set.ofList i) 
        |> Array.ofSeq

    m.Add(1, x.[0])
    m.Add(7, x.[1])
    m.Add(4, x.[2])
    m.Add(8, x.[9])
    let n3= [|x.[3];x.[4];x.[5]|]|> Array.find(fun d -> (Set.difference (d + m.[1]) d).IsEmpty)
    m.Add(3, n3)
    let n9 = m.[4]+m.[3]
    m.Add(9, n9)
    let l2 = m.[8] - m.[9]

    let n2= [|x.[3];x.[4];x.[5]|]|> Array.find(fun d -> (Set.difference (d + l2) d).IsEmpty)
    m.Add(2, n2)

    let n5= [|x.[3];x.[4];x.[5]|]|> Array.find(fun d -> d <> m.[2] && d <> m.[3])
    m.Add(5, n5)

    let n6 = n5 + l2
    m.Add(6, n6)

    let n0= [|x.[6];x.[7];x.[8]|]|> Array.find(fun d -> d <> m.[6] && d <> m.[9])
    m.Add(0, n0)

    let rm = 
        m
            |> Seq.map(fun p -> 
                // printfn "%i %s" p.Key (p.Value |> Set.toArray |> String)
                (p.Value, p.Key))
            |> Map.ofSeq

    let y =outputs 
        |> Seq.map(fun i -> Set.ofList i) 
        |> Seq.map(fun s -> rm.[s])
        |> Seq.fold(fun acc i -> acc * 10 + i) 0
    // printfn " - %i" y
    y

let setEach (a:string[]) =
    a
    |> Array.map(fun i -> i.ToCharArray() |> Array.sort |>List.ofArray)
    
inputs
|> Array.map(fun line -> line.Split(" | "))
|> Array.map(fun parts ->
    match parts with
    | [|signals;outputs|] -> (signals.Trim().Split(" ") |> setEach, outputs.Trim().Split(" ") |> setEach)
    | _ -> failwith "invalid input")
|> Array.map analyze
|> Array.sum
|> printfn "%i"