type Assignment = { Start: int; End: int }

let parseAssignment (assignment: string) : Assignment =
    let parts = assignment.Split "-"

    { Start = int parts[0]
      End = int parts[1] }

let parseLine (line: string) : Assignment * Assignment =
    let assignments = line.Split ","

    (parseAssignment assignments[0], parseAssignment assignments[1])

let assignmentsContained (elves: Assignment * Assignment) : bool =
    let (first, second) = elves

    (first.Start >= second.Start && first.End <= second.End)
    || (second.Start >= first.Start && second.End <= first.End)

let puzzleOne (lines: seq<string>) : int =
    lines |> Seq.map parseLine |> Seq.filter assignmentsContained |> Seq.length

let assignmentsOverlap (elves: Assignment * Assignment) : bool =
    let (first, second) = elves

    not (first.End < second.Start || second.End < first.Start)

let puzzleTwo (lines: seq<string>) : int =
    lines |> Seq.map parseLine |> Seq.filter assignmentsOverlap |> Seq.length

let lines = System.IO.File.ReadLines("input.txt")
puzzleOne lines |> printfn "%A"
puzzleTwo lines |> printfn "%A"
