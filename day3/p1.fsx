open System

let ToIntArray (s: string) :int[] = s.ToCharArray() |> Array.map(fun (c: char) -> int(c)-int('0'))
let ToInteger (a: int[]): int= Array.fold(fun (acc: int) (i: int) -> acc*2+i) 0 a

let inputs = IO.File.ReadAllLines("input.txt")

let reduction (acc: int[]) (i: int[]): int[]  =
    ( acc, i) ||> Array.map2(fun a x-> 
        match x with
        | 1 -> a+1
        | 0 -> a-1
        | x -> failwithf "unknow input %i" x
    )

// let s = "111100110001"
// s |> ToIntArray|> ToInteger |> printfn("%i:")
let d= inputs 
    |> Array.map ToIntArray


let gama = Array.reduce reduction d|> Array.map(fun i ->  if i >0 then 1 else 0) |> ToInteger
let epsilon =  Array.reduce reduction d |> Array.map(fun i ->  if i <=0 then 1 else 0) |> ToInteger

printfn("part1: %i") (gama * epsilon)