open System.IO

let testInput = 
    """11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124""".Split(",")

let input () =
    let currendDir = Directory.GetCurrentDirectory()
    printfn $"{currendDir}"
    let path = Path.Combine(currendDir, "input.txt")
    File.ReadAllText(path).Split(",")

let splitInHalf (s: string) =
    let half = s.Length / 2
    let a = s.Substring(0, half)
    let b = s.Substring(half)
    a, b
    
// SOMETHINGS WRONG, not done
let calc (xs : string array) =
    xs
    |> Array.map (fun s -> s.Trim())
    |> Array.map (fun s -> 
        let split = s.Split("-") 
        [int64 split.[0] .. int64 split.[1]])
    |> List.concat
    |> List.map string
    |> List.map (fun s ->
        let (f, s') = s |> splitInHalf
        (s, f = s')
    )
    |> List.filter snd
    |> List.map (fst >> int64)
    |> List.sum

// Test input works
testInput
|> calc

// Not working
input ()
|> calc

