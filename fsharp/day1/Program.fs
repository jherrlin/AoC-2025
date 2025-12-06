open System.IO

let startValue = 50

let testInput = "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"

let input  = 
    let path = Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    File.ReadAllText(path)

type Action =
    | L of int
    | R of int

let parse (s: string) =
    s.Trim().Split('\n')
    |> Array.map(fun s -> 
        let i = int s.[1..]
        match s.[0] with
        | 'L' -> L i
        | 'R' -> R i
        | _ -> failwith "Invalid action"
    )

let moveLeft (position : int) (value : int) =
    let newValue = position - value
    if newValue < 0 then
        100 + newValue
    else newValue

let moveRight (position : int) (value : int) =
    let newValue = position + value
    if 100 <= newValue then
        newValue - 100 
    else newValue


let calc actions =
    actions
    |> Array.fold (fun (position, counter) action ->
        let newPosition =
            match action with
            | L v -> moveLeft position v
            | R v -> moveRight position v
        let newCounter = if newPosition = 0 then counter + 1 else counter
        newPosition, newCounter
    ) (startValue, 0)

testInput
|> parse
|> calc

input
|> parse
|> calc

