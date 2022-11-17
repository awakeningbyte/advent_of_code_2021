open System
let inputs=IO.File.ReadAllLines("test.txt")

type Template = char[]

type Insert = ((char * char) * char)

let (|Template|Insert|) (s:string) =
    match s.ToCharArray() with
    | [|p1;p2; ' '; '-';'>';' '; t|] -> Insert ((p1,p2), t)
    | x -> Template x

let (template, inserts) =
    inputs
    |> Array.fold(fun (tp, ops) line ->
        match line with
        | Template t -> (t,ops)
        | Insert i -> (tp, [yield! ops; i])
    ) (Array.empty, [])