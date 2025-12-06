module Day3.Problem2

open System.IO

let input () =
    let currendDir = Directory.GetCurrentDirectory()
    printfn $"{currendDir}"
    let path = Path.Combine(currendDir, "input.txt")
    File.ReadAllLines(path)
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let testInput = 
    """987654321111111
811111111111119
234234234234278
818181911112111""".Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let parseBank (line: string) =
    line
    |> Seq.map (string >> int)
    |> Seq.toList

let rec calc acc nb (bank : int list) =
    if nb = 0 then acc
    else 
        let splitIndex = bank.Length - (nb - 1)
        let candidates = List.take splitIndex bank
        let (nextIdx, next) = candidates |> List.indexed |> List.maxBy snd
        let remainingBank = bank |> List.skip (nextIdx + 1)
        calc (acc @ [next]) (nb - 1) remainingBank

let solve xs =
    xs
    |> List.map parseBank
    |> List.map (calc [] 12)
    |> List.map (List.map string >> String.concat "" >> int64)
    |> List.sum


[<EntryPoint>]
let main argv =
    let testOut: int64 = testInput |> solve 
    let realOut: int64 = input () |> solve 
    printfn $"Test: {testOut}"
    printfn $"Real: {realOut}"
    0

// 171741365473332 is the correct value