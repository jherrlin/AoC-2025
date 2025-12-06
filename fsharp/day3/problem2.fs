open System.IO

let input () = 
    let path = Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
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

let rec solve acc nb (bank : int list) =
    if nb = 0 then acc |> List.rev
    elif nb = bank.Length then (acc |> List.rev) @ bank
    else 
        let splitIndex = bank.Length - (nb - 1)
        let candidates = List.take splitIndex bank
        let (idxnext, next) = candidates |> List.indexed |> List.maxBy snd
        let remainingBank = bank |> List.skip (idxnext + 1)
        solve (next :: acc) (nb - 1 ) remainingBank


let solver xs =
    xs
    |> List.map parseBank
    |> List.map (solve [] 12)
    |> List.map (List.map string >> String.concat "" >> int64)
    |> List.sum

testInput
|> solver

input ()
|> solver
// 171741365473332 is the correct value