open System


type Instruction = 
    |UP of int
    |DOWN of int
    |FORWARD of int

let parseInput (t: string) : Instruction=
    match t.Split(" ") with
        | [|"up"; v|] -> UP (int v)
        | [|"down"; v|] -> DOWN (int v)
        | [|"forward"; v|] -> FORWARD (int v)
        | _ -> failwith "unknow instruction"   

type Position = 
    {
        X: int
        Y:int
        Aim: int
    }

let start = {X=0; Y=0; Aim=0}
let inputs = IO.File.ReadAllLines("input1.txt")
                            |> Array.map parseInput
                            
let endPostion = (start, inputs) ||> Array.fold (fun acc  input -> 
    match input with
    | UP  v-> { acc with Y = (acc.Y-v)}
    | DOWN v -> { acc with Y = (acc.Y + v)}
    | FORWARD v -> {acc with X =(acc.X + v)})

printfn "part1: %i" (endPostion.X * endPostion.Y)

let endPostion2 = (start, inputs) ||> Array.fold (fun acc  input -> 
    match input with
    | UP  v-> { acc with Aim = (acc.Aim-v)}
    | DOWN v -> { acc with Aim = (acc.Aim + v)}
    | FORWARD v ->  {acc with X =(acc.X + v) ; Y =(acc.Y + (acc.Aim * v))})
printfn "part2: %i" (endPostion2.X * endPostion2.Y)