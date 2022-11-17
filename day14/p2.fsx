open System
open System.Collections.Generic
let inputs=IO.File.ReadAllLines("test.txt")

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
let mem = new Dictionary<char,int64>()

//build the count mem
template
|> Seq.groupBy(fun c -> c)
|> Seq.map( fun (c,sq )-> (c, Seq.length sq))
|> Seq.iter(fun (c, len) -> mem.Add(c, len))
|> ignore

let rec proc h t =
    if Seq.isEmpty h then
        printfn ("\t%A") (Seq.length t)
    match t with
    | x::y::tail when (lookup.ContainsKey (x,y)) ->
        let toAdd = lookup[(x,y)]
        if mem.ContainsKey(toAdd) then
            let org = mem.[toAdd]
            mem.[toAdd] <- (org + 1L)
        else
            mem.Add(toAdd, 1L)
    
        proc ([yield! h;x;toAdd]) [y; yield! tail]
    | x::y::tail -> 
        proc h [y; yield! tail]
    | [x] -> 
        [yield! h;x] 
    | [] -> h 

[1..10]
|> Seq.fold (fun tp _ ->
    proc List.empty<char> (List.ofSeq tp)
)  (List.ofArray template)

let m = mem.Values|> Seq.max
let s = mem.Values|> Seq.min

printfn "%A" (m - s)