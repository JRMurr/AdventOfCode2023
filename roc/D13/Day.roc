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

# For reflect funcs, the second arg means we are flipping "after" that row/col
# ie if 4 we flip btwn col/row 4,5

reflectOverRow : Index, Nat -> Index
reflectOverRow = \idx, flipRow ->
    { x: column, y: row } = idx
    if row <= flipRow then
        diff = (flipRow - row) + 1
        { x: column, y: flipRow + diff }
    else
        diff = (row - flipRow) - 1
        { x: column, y: flipRow - diff }

expect reflectOverRow { x: 0, y: 4 } 4 == { x: 0, y: 5 }
expect reflectOverRow { x: 0, y: 5 } 4 == { x: 0, y: 4 }
expect reflectOverRow { x: 0, y: 1 } 4 == { x: 0, y: 8 }
expect reflectOverRow { x: 0, y: 0 } 4 == { x: 0, y: 9 }

reflectOverCol : Index, Nat -> Index
reflectOverCol = \idx, flipCol ->
    { x: column, y: row } = idx
    if column <= flipCol then
        diff = flipCol - row + 1
        { x: flipCol + diff, y: row }
    else
        diff = row - flipCol - 1
        { x: flipCol - diff, y: row }

validFlip : Pattern, Tile, Index -> Bool
validFlip = \pattern, ogVal, flippedIdx ->
    when Array2D.get pattern flippedIdx is
        Ok vFlip -> ogVal == vFlip
        Err _ -> Bool.true

findRowReflect : Pattern -> Result Index [NotFound]
findRowReflect = \pattern ->
    { dimX: numCols, dimY: numRows } = Array2D.shape pattern

    # dbg {numCols, numRows}

    possibleColFlip = numCols // 2

    # TODO: only need to walk up to the flip idx
    validColFlip = Array2D.walkUntil
        pattern
        Bool.true
        { direction: Forwards }
        (\_, val, idx ->
            flipped = reflectOverCol idx possibleColFlip
            if validFlip pattern val flipped then
                Continue Bool.true
            else
                dbg
                    { val, idx, flipped }

                Break Bool.false
        )

    dbg
        validColFlip

    Err NotFound

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    patterns = parsePatterns in

    dbg
        patterns |> List.map findRowReflect

    Err NotImplemented

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
