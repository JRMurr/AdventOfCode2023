interface D12.Day
    exposes [solution]
    imports [
        AoC,
        Parser.Core.{ Parser, oneOf, oneOrMore, const, skip, keep, sepBy },
        Parser.String.{ parseStr, scalar, digits },
    ]

solution : AoC.Solution
solution = { day: 12, part1, part2 }

Spring : [Working, Broken, Unknown]

Row : {
    springs : List Spring,
    damagedSizes : List Nat,
}

parseRows : Str -> Result (List Row) Str
parseRows = \s ->
    spring = oneOf [
        const Working |> skip (scalar '.'),
        const Broken |> skip (scalar '#'),
        const Unknown |> skip (scalar '?'),
    ]

    damagedSizesP = digits |> sepBy (scalar ',')

    row =
        const (\springs -> \damagedSizes -> { springs, damagedSizes })
        |> keep (oneOrMore spring)
        |> skip (scalar ' ')
        |> keep damagedSizesP

    rows = row |> sepBy (scalar '\n')

    when parseStr rows (Str.trim s) is
        Ok res -> Ok res
        Err (ParsingFailure e) -> Err "ParsingFailure: \(e)"
        Err (ParsingIncomplete e) -> Err "ParsingIncomplete: \(e)"



part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in -> 
    dbg 
        (parseRows in)
    Err NotImplemented

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
