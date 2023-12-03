interface D03.Day
    exposes [solution]
    imports [AoC, Util]

solution : AoC.Solution
solution = { day: 3, part1, part2 }

Cord : { x : Nat, y : Nat }{}

toCord : Nat, Nat -> Cord
toCord = \row, col -> { y: row, x: col }

GridVal : [Empty, Digit U64, Symbol Str]

charToGridVal : Str -> GridVal
charToGridVal = \s ->
    when s is
        "." -> Empty
        x if Result.isOk (Str.toNat x) -> Digit (Util.toU64Unsafe x)
        y -> Symbol y

Grid : Dict Cord Str

parseLine : Str, Nat -> List ({ x : Nat, y : Nat }, GridVal)
parseLine = \s, row ->
    Str.graphemes s
    |> List.mapWithIndex
        (\c, idx ->
            (toCord row idx, charToGridVal c)
        )

# parseGrid : Str -> Grid
parseGrid = \s ->
    Str.split s "\n"
    |> List.mapWithIndex (\line, row -> parseLine line row)
    |> Util.flatten
    |> Dict.fromList

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    # grid : Grid
    grid = parseGrid in

    dbg
        grid

    Ok "sad"

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
