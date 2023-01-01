let sharedItemOfElf (rucksack: string) : char =
    let compartments =
        rucksack.ToCharArray() |> Array.splitInto 2 |> Array.map Set.ofArray

    Set.intersect compartments[0] compartments[1] |> Set.toList |> List.head

let priorityOfItem (item: char) : int =
    let item = int item
    if item <= int 'Z' then item - 38 else item - 96

let puzzleOne (lines: seq<string>) : int =
    lines |> Seq.map sharedItemOfElf |> Seq.map priorityOfItem |> Seq.sum

let sharedItemOfGroup (group: string[]) : char =
    Set.intersectMany (seq { for elf in group -> elf.ToCharArray() |> Set.ofArray })
    |> Set.toList
    |> List.head

let puzzleTwo (lines: seq<string>) : int =
    lines
    |> Seq.chunkBySize 3
    |> Seq.map sharedItemOfGroup
    |> Seq.map priorityOfItem
    |> Seq.sum


let lines = System.IO.File.ReadLines("input.txt")
puzzleOne lines |> printfn "%A"
puzzleTwo lines |> printfn "%A"
