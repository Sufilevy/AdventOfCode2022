type Move =
    | Rock
    | Paper
    | Scissors

let parseMove (move: string) =
    match move with
    | "A"
    | "X" -> Rock
    | "B"
    | "Y" -> Paper
    | _ -> Scissors

let scoreMove (move: Move) =
    match move with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

module PuzzleOne =

    let compareMoves (opponent: Move, you: Move) =
        if opponent = you then
            3
        else
            match opponent with
            | Rock -> if you = Paper then 6 else 0
            | Paper -> if you = Scissors then 6 else 0
            | Scissors -> if you = Rock then 6 else 0

    let ScoreRound (line: string) =
        let moves = line.Split " "
        let opponent, you = (parseMove moves[0], parseMove moves[1])
        compareMoves (opponent, you) + scoreMove you

let puzzleOne (input: seq<string>) =
    input |> Seq.map PuzzleOne.ScoreRound |> Seq.sum

module PuzzleTwo =
    type Action =
        | Lose
        | Draw
        | Win

    let parseAction (action: string) =
        match action with
        | "X" -> Lose
        | "Y" -> Draw
        | _ -> Win

    let getMoveFromAction (action: Action, opponent: Move) =
        if action = Draw then
            opponent
        else
            match opponent with
            | Rock -> if action = Win then Paper else Scissors
            | Paper -> if action = Win then Scissors else Rock
            | Scissors -> if action = Win then Rock else Paper

    let scoreAction (action: Action) =
        match action with
        | Lose -> 0
        | Draw -> 3
        | Win -> 6

    let ScoreRound (line: string) =
        let moves = line.Split " "
        let opponent, action = (parseMove moves[0], parseAction moves[1])
        let you = getMoveFromAction (action, opponent)
        scoreAction action + scoreMove you

let puzzleTwo (input: seq<string>) =
    input |> Seq.map PuzzleTwo.ScoreRound |> Seq.sum

let lines = System.IO.File.ReadLines("input.txt")
puzzleOne lines |> printfn "%A"
puzzleTwo lines |> printfn "%A"
