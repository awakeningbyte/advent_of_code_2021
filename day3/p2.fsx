open System

let ToIntArray (s: string) :int[] = s.ToCharArray() |> Array.map(fun (c: char) -> int(c)-int('0'))
let ToInteger (a: int[]): int= Array.fold(fun (acc: int) (i: int) -> acc*2+i) 0 a

let inputs = IO.File.ReadAllLines("input.txt")

let d= inputs 
    |> Array.map ToIntArray
type SIGNAL = |GAMA|EPSILON

let rec Process (i: int) (arr: int[][]) (t: SIGNAL)=
    if i >= arr.Length || arr.Length = 1 then
        arr[0]
    else
        let g =arr 
            |> Array.groupBy(fun a -> a.[i])
        let (_, ones) = g[0]
        let (_, zeros: int[][]) = g[1]
        match t with
        | GAMA when ones.Length > zeros.Length->  Process (i+1) ones t 
        | GAMA when ones.Length <= zeros.Length->  Process (i+1) zeros t 
        | EPSILON when ones.Length >= zeros.Length->  Process (i+1) zeros t 
        | EPSILON when ones.Length < zeros.Length->  Process (i+1) ones t 
let gama = Process 0 d GAMA |> ToInteger
let epsilon = Process 0 d EPSILON |> ToInteger

printfn("part2: %i") (gama * epsilon)