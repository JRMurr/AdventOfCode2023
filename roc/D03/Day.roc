interface D03.Day
    exposes [solution]
    imports [
        AoC,
        Util,
        Coord.{ Coord, neighbors },
        Array2D.{ Array2D },
    ]

solution : AoC.Solution
solution = { day: 3, part1, part2 }

toCord : Nat, Nat -> Coord
toCord = \row, col -> { y: row, x: col }

GridVal : [Empty, Digit U64, Symbol Str]

charToGridVal : Str -> GridVal
charToGridVal = \s ->
    when s is
        "." -> Empty
        x if Result.isOk (Str.toNat x) -> Digit (Util.toU64Unsafe x)
        y -> Symbol y

Grid : Array2D GridVal

parseLine : Str -> List GridVal
parseLine = \s ->
    Str.graphemes s
    |> List.map (charToGridVal)

parseGrid : Str -> Array2D GridVal
parseGrid = \s ->
    Str.split s "\n"
    |> List.map parseLine
    |> Array2D.fromLists FitShortest
# |> Util.flatten
# |> Dict.fromList

getGridVal : Coord, Grid -> GridVal
getGridVal = \c, grid ->
    when Array2D.get grid c is
        Ok v -> v
        Err _ -> Empty

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    grid : Grid
    grid = parseGrid in

    dbg
        getGridVal { x: 0, y: 0 } grid

    Ok "sad"

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
