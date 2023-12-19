open System.Text.RegularExpressions

let input = stdin.ReadToEnd().Split '\n'

let sum (input: string array) =
    input
    |> Array.map (fun s -> Regex.Replace (s, @"[^0-9]", ""))
    |> Array.filter (fun s -> String.length (s) <> 0)
    |> Array.map (fun s ->
        let l = String.length (s)
        s.Substring (0,1) + s.Substring (l-1))
    |> Array.map (fun s -> int s)
    |> Array.fold (fun acc v -> acc+v) 0

let replSpelledOut (str: string) =
    let digits = [| "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten" |]
    let mutable out = str
    for n in 0..digits.Length-1 do
        let fc = digits[n].Substring (0,1)
        let lc = digits[n].Substring (digits[n].Length-1)
        out <- out.Replace (digits[n], fc + string (n+1) + lc)
    out

printfn "part 1: %d" (input |> sum)
printfn "part 2: %d" (input |> Array.map replSpelledOut |> sum)