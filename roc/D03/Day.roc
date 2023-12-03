interface D03.Day
    exposes [solution]
    imports [
        AoC,
        Util,
        Coord.{ getNeighbors },
        Array2D.{ Index, Array2D },
    ]

solution : AoC.Solution
solution = { day: 3, part1, part2 }

toCord : Nat, Nat -> Index
toCord = \row, col -> { y: row, x: col }

GridVal : [Empty, Digit U64, Symbol Str]

valIsDigit = \x ->
    when x is
        Digit _ -> Bool.true
        _ -> Bool.false

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
    lines = Str.split s "\n"
        |> List.map parseLine

    Array2D.fromLists lines FitShortest

getGridVal : Index, Grid -> GridVal
getGridVal = \c, grid ->
    when Array2D.get grid c is
        Ok v -> v
        Err _ -> Empty

getCoordsWithPred : Grid, (GridVal -> Bool) -> List Index
getCoordsWithPred = \grid, fn ->
    Array2D.walk
        grid
        []
        { direction: Forwards }
        (\acc, val, c ->
            if fn val then List.append acc c else acc
        )

# Given a coord return indexes for digits that are neighbors
neighborDigits : Grid, Index -> List Index
neighborDigits = \grid, index ->
    getNeighbors index
    |> List.keepIf (\i -> valIsDigit (getGridVal i grid))

# given an index of a digit walk forward and back to build up the number
collectDigits : Grid, Index -> U64
collectDigits = \grid, index ->

    reducer : List U64, GridVal, Index -> [Continue (List U64), Break (List U64)]
    reducer = \acc, val, _ ->
        when val is
            Digit x -> Continue (List.append acc x)
            _ -> Break acc

    leftWalk = Array2D.walkUntil grid [] { direction: Backwards, start: index } reducer
    rightWalk = Array2D.walkUntil grid [] { direction: Forwards, start: Coord.east index } reducer

    digits = List.concat leftWalk rightWalk

    toNum : U64, U64, Nat -> U64
    toNum = \acc, d, idx ->
        idxU64 = Num.toU64 idx
        (idxU64 * 10) + acc + d

    List.reverse digits
    |> List.walkWithIndex 0u64 toNum

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    grid = parseGrid in

    symbolPoints = getCoordsWithPred
        grid
        (\v ->
            when v is
                Symbol _ -> Bool.true
                _ -> Bool.false
        )

    # TODO: the same number can have its start in here multiple times..
    startDigits =
        List.map symbolPoints (\p -> neighborDigits grid p)
        |> Util.flatten
    
    validNums = List.map startDigits (\i -> collectDigits grid i)

    dbg startDigits

    Ok "sad"

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
