interface D13.Day
    exposes [solution]
    imports [
        AoC,
        Util,
        Array2D.{ Index, Array2D },
    ]

solution : AoC.Solution
solution = { day: 13, part1, part2 }

Tile : [Ash, Rock]

Pattern : Array2D Tile

patternParser =
    tile = Util.tileParser [(Ash, '.'), (Rock, '#')]
    Util.get2DParser tile

parsePatterns : Str -> List Pattern
parsePatterns = \s -> Util.parseSepEmptyLine s patternParser |> Util.unwrapS

reflectOverRow : Index, Nat -> Index
reflectOverRow = \idx, flipRow ->
    { x: column, y: row } = idx
    if row <= flipRow then
        diff = flipRow - row
        { x: column, y: flipRow + diff }
    else
        # we should only check one side, crash if we get here to save time
        crash "index has row greater than flip"

reflectOverCol : Index, Nat -> Index
reflectOverCol = \idx, flipCol ->
    { x: column, y: row } = idx
    if column <= flipCol then
        diff = flipCol - row
        { x: flipCol + diff, y: row }
    else
        # we should only check one side, crash if we get here to save time
        crash "index has column greater than flip"

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    dbg
        parsePatterns in

    Err NotImplemented

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
