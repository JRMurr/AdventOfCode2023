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

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    dbg
        parsePatterns in

    Err NotImplemented

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
