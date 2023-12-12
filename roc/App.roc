interface App
    exposes [
        findSolution,
        solutions,
        solvePuzzle,
    ]
    imports [
        AoC,
        D01.Day,
        D02.Day,
        D03.Day,
        D04.Day,
        D05.Day,
        D06.Day,
        D07.Day,
        D08.Day,
        D09.Day,
        D10.Day,
        D11.Day,
        # -- Add day import
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
    D02.Day.solution,
    D03.Day.solution,
    D04.Day.solution,
    D05.Day.solution,
    D06.Day.solution,
    D07.Day.solution,
    D08.Day.solution,
    D09.Day.solution,
    D10.Day.solution,
    D11.Day.solution,
    # -- Add day List
]
