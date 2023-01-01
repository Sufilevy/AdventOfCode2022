let caloriesOf (elf: string) =
    elf.Split '\n'
    |> Array.filter ((<>) "")
    |> Array.map int
    |> Array.sum

let puzzleOne (input: string) = 
    (input.Split "\n\n")
    |> Array.map caloriesOf
    |> Array.max

let puzzleTwo (input: string) =
    (input.Split "\n\n")
    |> Array.map caloriesOf
    |> Array.sortDescending
    |> Array.take 3
    |> Array.sum

let file = System.IO.File.ReadAllText("input.txt")
puzzleOne file |> printfn "%A"
puzzleTwo file |> printfn "%A"