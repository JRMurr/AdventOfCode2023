interface App
    exposes [
        findSolution,
        solutions,
        solvePuzzle,
    ]
    imports [
        AoC,
        D01.Day,
    ]

filterByDay : U8 -> (AoC.Solution -> Result AoC.Solution [DoesNotMatch])
filterByDay = \day ->
    \sol ->
        if sol.day == day then
            Ok sol
        else
            Err DoesNotMatch

findSolution : U8 -> Result AoC.Solution [SolutionNotFound]
findSolution = \day ->
    List.keepOks solutions (filterByDay day)
    |> List.first
    |> Result.mapErr (\_ -> SolutionNotFound)

# TODO: will some puzzles not need to read input?
solvePuzzle : { solution : AoC.Solution, puzzle : [Part1, Part2], input : Str } -> Result Str [NotImplemented, Error Str]
solvePuzzle = \selection ->
    func =
        when selection.puzzle is
            Part1 -> selection.solution.part1
            Part2 -> selection.solution.part2

    func selection.input

## Export a list of the solutions included in this app
solutions : List AoC.Solution
solutions = [
    D01.Day.solution,
]
