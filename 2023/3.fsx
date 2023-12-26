open System.Text.RegularExpressions

let input =
    stdin.ReadToEnd().Split '\n'
    |> Array.map (fun s -> s.ToCharArray ())

let mutable symbols: (int*int*char) list = List.empty<int*int*char>
let mutable numbers: (int*int*int*int) list = List.empty<int*int*int*int>

for y in 0..input.Length-1 do
    let mutable start = -1
    for x in 0..input[y].Length-1 do
        let c = input[y][x]
        if c >= '0' && c <= '9' then
            if start = -1 then start <- x
        else
            if c <> '.' then symbols <- (x,y,c) :: symbols
            if start >= 0 then
                numbers <- (start, x-1, y, int(System.String(input[y][start..x-1]))) :: numbers
                start <- -1
    if start <> -1 then
        numbers <- (start, input.Length-1, y, int(System.String(input[y][start..input.Length-1]))) :: numbers

let checkIfNumberTouchesSymbol (symbol: int*int*char) (number: int*int*int*int) =
    let sx, sy, _ = symbol
    let nx1, nx2, ny, _ = number
    sx >= nx1-1 && sx <= nx2+1 && sy >= ny-1 && sy <= ny+1

let rec checkIfNumberTouchesAnySymbol (symbols:(int*int*char) list) (number: int*int*int*int) =
    match symbols with
    | [] -> false
    | head :: [] -> checkIfNumberTouchesSymbol head number
    | head :: tail ->
        checkIfNumberTouchesSymbol head number || checkIfNumberTouchesAnySymbol tail number

let rec numbersThatTouchSymbol (symbol: (int*int*char)) (numbers: (int*int*int*int) list) =
    List.filter (checkIfNumberTouchesSymbol symbol) numbers

let partNumbers =
    numbers
    |> List.filter (checkIfNumberTouchesAnySymbol symbols)

let gears =
    symbols
    |> List.filter (fun (x,y,c) -> c = '*' )
    |> List.choose (fun symbol -> 
        match numbersThatTouchSymbol symbol partNumbers with
        | [(_,_,_,a); (_,_,_,b) ] -> Some(symbol,a*b)
        | _ ->  None )

printfn "*** part 1: %d" (partNumbers |> List.map (fun (_,_,_,x) -> x) |> List.sum)
printfn "*** part 2: %d" (gears |> List.map (fun (symbol,ratio) -> ratio) |> List.sum)


