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

PartNumber : {
    digits : List U64,
    indices : List Array2D.Index,
}

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

isOverlapping : List a, List a -> Bool where a implements Eq
isOverlapping = \a, b ->
    List.any a \elemA ->
        List.any b \elemB ->
            elemA == elemB

getAllNumbers : Grid -> List PartNumber
getAllNumbers = \grid ->
    symbolPoints = getCoordsWithPred
        grid
        (\v ->
            when v is
                Symbol _ -> Bool.true
                _ -> Bool.false
        )
    allowedPoints =
        List.map symbolPoints (\p -> neighborDigits grid p)
        |> Util.flatten

    newPartNumber = { digits: [], indices: [] }
    startState = { candidates: [], current: newPartNumber }
    finalState = Array2D.walk
        grid
        startState
        { direction: Forwards }
        \{ candidates, current }, elem, index ->
            when elem is
                Digit d if !(Array2D.isRowEnd grid index) ->
                    {
                        candidates,
                        current: {
                            digits: List.append current.digits d,
                            indices: List.append current.indices index,
                        },
                    }

                Digit d ->
                    newCurr = {
                        digits: List.append current.digits d,
                        indices: List.append current.indices index,
                    }

                    {
                        candidates: List.append candidates newCurr,
                        current: newPartNumber,
                    }

                _ ->
                    {
                        candidates: List.append candidates current,
                        current: newPartNumber,
                    }

    finalState.candidates
    |> List.append finalState.current
    |> List.keepIf \candidate ->
        isOverlapping candidate.indices allowedPoints

# Given a coord return indexes for digits that are neighbors
neighborDigits : Grid, Index -> List Index
neighborDigits = \grid, index ->
    getNeighbors index
    |> List.keepIf (\i -> valIsDigit (getGridVal i grid))

digitsToNum = \digits ->
    toNum : U64, U64, Nat -> U64
    toNum = \acc, d, idx ->
        idxU64 = Num.toU64 idx
        mult = Num.powInt 10 idxU64
        acc + (mult * d)

    List.reverse digits
    |> List.walkWithIndex 0u64 toNum

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    grid = parseGrid in

    valid = getAllNumbers grid

    sum =
        valid
        |> List.walk 0 (\acc, x -> acc + (digitsToNum x.digits))

    Ok (Num.toStr sum)

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \in ->
    grid = parseGrid in
    valid = getAllNumbers grid

    gears = getCoordsWithPred
        grid
        (\v ->
            when v is
                Symbol "*" -> Bool.true
                _ -> Bool.false
        )

    List.map gears (\gear -> neighborDigits grid gear)
    |> List.map
        (\allowedPoints ->
            List.keepIf valid \candidate ->
                isOverlapping candidate.indices allowedPoints
        )
    |> List.keepIf (\lst -> List.len lst == 2)
    |> List.map
        (\parts ->
            List.map parts (\x -> digitsToNum x.digits)
            |> List.product
        )
    |> List.sum
    |> Num.toStr
    |> Ok
