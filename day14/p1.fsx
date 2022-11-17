open System
let inputs=IO.File.ReadAllLines("input.txt")

type Template = char[]
type Insert = ((char * char) * char)

let (|Template|Insert|) (s:string) =
    match s.ToCharArray() with
    | [|p1;p2; ' '; '-';'>';' '; t|] -> Insert ((p1,p2), t)
    | x -> Template x

let (template, inserts) =
    inputs
    |> Array.filter(fun line -> not (String.IsNullOrWhiteSpace(line)))
    |> Array.fold(fun (tp, ops) line ->
        match line with
        | Template t -> (t,ops)
        | Insert i -> (tp, [yield! ops; i])
    ) (Array.empty, [])

// printfn "%A %A" template inserts
let lookup = inserts |> dict

let proc tplt =
    [yield! tplt; '*']
    |> Seq.pairwise
    |> Seq.map(fun p ->
        match p with
        | (x,y) when (lookup.ContainsKey (x,y)) -> [x; lookup.[p]]
        | (x,_) -> [x]
    ) 
    |> Seq.collect(fun s -> s)

[1..10]
|> Seq.fold(fun t _ ->proc t) template
|> Seq.toArray
|> Seq.groupBy(fun c -> c)
|> Seq.sortBy(fun (c,g) -> Seq.length g)
|> fun result ->
    let (_, l) = result |> Seq.maxBy( fun (_,g) -> Seq.length g)
    let (_, s) = result |> Seq.minBy( fun (_,g) -> Seq.length g)
    (Seq.length l) - (Seq.length s)
|> printfn("%i")


// |> String
// |> printfn "%s"