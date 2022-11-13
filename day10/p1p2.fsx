open System
let inputs = IO.File.ReadAllLines("input.txt")
// ): 3 points.
// ]: 57 points.
// }: 1197 points.
// >: 25137 points.
let lefts = [|'{';'[';'('; '<'|]
let rights = [|'}';']';')'; '>'|]
let dict = 
  Array.map2 (fun l r -> (l, r)) lefts rights
  |> Map.ofArray

let rec check (line: list<char>) (left: list<char>) =
  match line with
  | [] -> (None,left)
  | h::remain when (dict.ContainsKey h) -> 
    check remain [h; yield! left;]
  | h::remain when left.IsEmpty -> 
    (Some(h),left)
  | h::remain when h = dict[(left.[0])] -> 
    check remain left[1..]
  | h::remain ->
    (Some(h), left)

let chunks = 
  inputs
  |> Array.map (fun i ->
      i.ToCharArray() |> List.ofArray
    )
  |> Array.map(fun a -> (check a []))

chunks
  |> Array.map(fun (h, _) -> 
    match h with
    | Some(x) ->
      match x with
      | ')' -> 3
      | ']' -> 57
      | '}' -> 1197
      | '>' -> 25137
      | _ -> failwithf "invalid input %c" x
    | None -> 0
  )
  |> Array.sum
  |> printfn "p1: %i"

chunks
  |> Array.filter (fun (h, _) -> h.IsNone)
  |> Array.map(fun (_, left) -> 
    // printf "%A\n" left
    left 
    |> Seq.fold(fun acc i->
      let r = 
        match i with
        | '(' -> 1L
        | '[' -> 2L
        | '{' -> 3L
        | '<' -> 4L
        | _ -> failwithf "invalid input %c" i
      r + acc * 5L
    ) 0L
  )
  |> Array.sort
  |> fun a->
    let l = a.Length / 2
    printfn "p2: %i" a.[l]
