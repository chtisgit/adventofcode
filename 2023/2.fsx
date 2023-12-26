open System.Text.RegularExpressions

let RedCubes = 12
let GreenCubes = 13
let BlueCubes = 14

let input = stdin.ReadToEnd().Split '\n'

type Comb = struct  
    val id : int
    val red: int
    val green: int
    val blue: int
    new (id, red, green, blue) =
        { id = id; red = red; green = green; blue = blue; }
end

let getID (line: string) =
    let m = Regex(@"^Game ([0-9]+)").Match(line)
    m.Groups[1].Value

let getMax (line: string) (color: string) =
    let mm = Regex(@"([0-9]+) "+color).Matches(line)
    let mutable max = 0
    for m in mm do
        let v = int m.Groups[1].Value
        if v > max then max <- v
    max

let getGames (input: string array) =
    input
    |> Array.filter (fun line -> (getID line) <> "")
    |> Array.map (fun line ->
        let id = int (getID line)
        let red = getMax line "red"
        let green = getMax line "green"
        let blue = getMax line "blue"
        Comb(id, red, green, blue)
        )

let part1 (games: Comb array) =
    games
    |> Array.fold (fun acc game ->
        if game.red <= RedCubes && game.green <= GreenCubes && game.blue <= BlueCubes then acc + game.id
        else acc) 0

let part2 (games: Comb array) =
    games
    |> Array.fold (fun acc game -> acc + game.red*game.green*game.blue) 0

let games = input |> getGames
printfn "*** part 1: %d" (games |> part1) 
printfn "*** part 2: %d" (games |> part2) 
