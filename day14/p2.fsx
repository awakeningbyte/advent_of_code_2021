open System
open System.Collections.Generic
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

let lookup = inserts |> dict

let start =
    [yield! template;'*']
    |> Seq.pairwise
    |> Seq.groupBy(fun p -> p)
    |> Seq.map(fun (k,sq) -> (k, int64 (Seq.length sq)))

let proc seqPair:seq<(char*char)*int64> =
    seqPair
    |> Seq.map(fun (pair,  cnt) -> 
            if lookup.ContainsKey pair then
                let ins = lookup[pair]
                let (x,y) = pair
                [((x,ins), cnt); ((ins,y), cnt)]
            else
                [(pair,cnt)]
        )
    |> Seq.collect(fun i -> i)
    |> Seq.groupBy(fun (p, cnt) -> p)
    |> Seq.map(fun (k, sq) ->
        let tlt =
            sq
            |> Seq.sumBy(fun (_, cnt) -> cnt)
        (k,tlt)
    )

let result =
    [1..40]
    |> Seq.fold (fun st _ ->
        proc st
    )   start
    |> Seq.map(fun ((x,y), cnt) -> 
            (x, cnt)
        )
    |> Seq.groupBy(fun (x, cnt) -> x)
    |> Seq.map(fun (x,sq) ->
        sq
        |> Seq.map(fun (_, cnt) -> 
            cnt
            )
        |> Seq.sum
    )

let m = Seq.max result
let s = Seq.min result

// let s = mem.Values|> Seq.min

printfn "%A" (m - s)