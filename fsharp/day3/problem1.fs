open System.IO

let input () = 
    let path = Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    File.ReadAllText(path).Split("\n")
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

let pairs bank =
    [for (i, b1) in bank |> List.indexed do
     for j in (i + 1) .. (bank |> Seq.length) - 1 do
        let b2 = bank[j]
        b1, b2]

let joltage bank =
    bank
    |> pairs
    |> List.map ((fun (b1,b2) -> $"{b1}{b2}") >> int)
    |> List.max

let solve input =
    let banks = input |> List.map parseBank
    banks |> List.map joltage |> List.sum

testInput
|> solve 

input ()
|> solve
// 17316 is the correct value