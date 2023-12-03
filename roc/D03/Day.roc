interface D03.Day
    exposes [solution]
    imports [
        AoC,
        Util,
        Coord.{ getNeighbors },
        Array2D.{ Index, Array2D },
        "in.example" as exampleInput : Str,
    ]

solution : AoC.Solution
solution = { day: 3, part1, part2 }

# toCord : Nat, Nat -> Index
# toCord = \row, col -> { y: row, x: col }

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
    lines =
        Str.split s "\n"
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

# if two digits are next to each, "merge"
#  them to only return 1 index for each number instead of two
combineDigitIndex : List Index -> List Index
combineDigitIndex = \lst ->

    List.sortWith lst (\a,b -> 
        xComp = Num.compare a.x b.x
        if xComp == EQ then Num.compare a.y b.y else xComp
    )
    |> List.walk [] (\acc, val -> 
        # if List.len acc == 0 then List.append acc val
        # else
        when acc is 
        # to the left/right of curr so no need to add
        [.., last] if Coord.west val == last -> acc
        # [.., last] if Coord.east val == last -> acc
        _ -> List.append acc val
    )


# given an index of a digit walk forward and back to build up the number
collectDigits : Grid, Index -> U64
collectDigits = \grid, index ->
    
    reducer : List U64, GridVal, Index -> [Continue (List U64), Break (List U64)]
    reducer = \acc, val, _ ->
        when val is
            Digit x -> Continue (List.append acc x)
            _ -> Break acc


    # 
    leftWalk = Array2D.walkUntil grid [] { direction: Backwards, start: index } reducer |> List.reverse
    rightWalk = Array2D.walkUntil grid [] { direction: Forwards, start: Coord.east index } reducer

    digits = List.concat leftWalk rightWalk

    toNum : U64, U64, Nat -> U64
    toNum = \acc, d, idx ->
        idxU64 = Num.toU64 idx
        mult = Num.powInt 10 idxU64 
        acc + (mult * d)

    List.reverse digits
    |> List.walkWithIndex 0u64 toNum

# exampleGrid = parseGrid exampleInput

# expect collectDigits exampleGrid { x: 0, y: 1 } == 467

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

    dbg Array2D.shape grid

    # TODO: the same number can have its start in here multiple times..
    startDigits =
        List.map symbolPoints (\p -> neighborDigits grid p)
        |> Util.flatten
        |> combineDigitIndex

    # dbg startDigits

    valid = List.map startDigits (\i -> collectDigits grid i)

    dbg valid 

    sum = 
        valid
        |> List.walk 0 (\acc, x -> acc + x)
    
    
    Ok (Num.toStr sum)

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
