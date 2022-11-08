open System
let inputs=IO.File.ReadAllLines("input.txt")

let herd = inputs.[0].Split(',')
        |> Array.map int
        |> Seq.ofArray
        |> Seq.groupBy (fun h -> h)
        |> Seq.map(fun (key, n) -> (key, int64(Seq.length n)))
        |> Map.ofSeq

let move (acc: Map<int,int64>) (i: int) =
    acc
    // |> Map.iter(fun (k:int) (v) -> printfn "%i:%i" k v)
    |> Map.fold (fun (m: Map<int,int64>) (k:int) (v: int64)->
        match k with 
        | 0 ->
            let n =
                if m.ContainsKey(6) then
                    m.Add(6, m.[6] + v)
                else
                    m.Add(6, v)
            n.Add(8, v)
        | 7 ->
            if m.ContainsKey(6) then
                m.Add(6, m.[6]+v)
            else
                m.Add(6, v)
        | x -> m.Add((x - 1), v)
        
    ) Map.empty<int, int64>
 
{1..256}
|> Seq.fold move herd
|> Seq.sumBy(fun k  -> k.Value)
|> printfn "%i"