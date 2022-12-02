open System
let inputs=IO.File.ReadAllLines("test.txt")


let grid =
    inputs
    |> Array.map(fun line -> 
        line.ToCharArray()
        |> Array.map int
    )

let X = (Array.length grid[0])
let Y = (Array.length grid)

type Point  = { X: int; Y: int }

let init = (X * Y * 9) - 9
//GEGGCI0KXXCQ