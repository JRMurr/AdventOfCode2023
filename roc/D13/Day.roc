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

relfectAxis : [Row, Col] -> (Index, Nat -> Index)
relfectAxis = \axis -> \idx, dim ->
        val = if axis == Row then idx.y else idx.x
        isEven = dim % 2 == 0
        if
            isEven && val == dim
        then
            idx
        else
            flipIdx = dim // 2
            offset = if isEven then 0 else 1

            newVal = (
                if val <= flipIdx then
                    diff = ((flipIdx - val) + offset)
                    flipIdx + diff
                else
                    diff = (val - flipIdx) - offset
                    flipIdx - diff
            )

            when axis is
                Row -> { y: newVal, x: idx.x }
                Col -> { y: idx.y, x: newVal }

reflectOverRow : Index, Nat -> Index
reflectOverRow = relfectAxis Row

expect reflectOverRow { x: 0, y: 4 } 9 == { x: 0, y: 5 }
expect reflectOverRow { x: 0, y: 5 } 9 == { x: 0, y: 4 }
expect reflectOverRow { x: 0, y: 1 } 9 == { x: 0, y: 8 }
expect reflectOverRow { x: 0, y: 0 } 9 == { x: 0, y: 9 }

expect reflectOverRow { x: 0, y: 6 } 14 == { x: 0, y: 8 }
expect reflectOverRow { x: 0, y: 7 } 14 == { x: 0, y: 7 }
expect reflectOverRow { x: 0, y: 8 } 14 == { x: 0, y: 6 }

reflectOverCol : Index, Nat -> Index
reflectOverCol = relfectAxis Col

expect reflectOverCol { y: 0, x: 4 } 9 == { y: 0, x: 5 }

validFlip : Pattern, Tile, Index -> Bool
validFlip = \pattern, ogVal, flippedIdx ->
    when Array2D.get pattern flippedIdx is
        Ok vFlip -> ogVal == vFlip
        Err _ -> Bool.true

findReflect : Pattern -> Result Nat [NotFound]
findReflect = \pattern ->
    { dimX: numCols, dimY: numRows } = Array2D.shape pattern

    checkFlip = \flipper, dim ->
        Array2D.walkUntil
            pattern
            Bool.true
            { direction: Forwards }
            (\_, val, idx ->
                flipped = flipper idx dim
                if validFlip pattern val flipped then
                    Continue Bool.true
                else
                    # dbg
                    #     { val, idx, flipped, flipIdx }
                    Break Bool.false
            )

    # dbg {numCols, numRows}

    possibleColFlip = numCols // 2

    # TODO: only need to walk up to the flip idx
    validColFlip = checkFlip reflectOverCol numCols
    # Array2D.walkUntil
    #     pattern
    #     Bool.true
    #     { direction: Forwards }
    #     (\_, val, idx ->
    #         flipped = reflectOverCol idx possibleColFlip
    #         if validFlip pattern val flipped then
    #             Continue Bool.true
    #         else
    #             # dbg
    #             #     { val, idx, flipped, possibleColFlip }
    #             Break Bool.false
    #     )

    if validColFlip then
        Ok ((possibleColFlip + 1) * 100)
    else
        possibleRowFlip = numRows // 2

        validRowFlip = checkFlip reflectOverRow numRows

        if validRowFlip then Ok ((possibleRowFlip + 1)) else Err NotFound

findReflectUnsafe = \pattern ->

    when findReflect pattern is
        Ok x -> x
        Err _ ->
            dbg
                Util.drawArray
                    pattern
                    (\t, _ ->
                        if t == Ash then "." else "#"
                    )

            crash "bad pattern"

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    patterns = parsePatterns in

    patterns # |> List.map findReflectUnsafe
    |> List.keepOks findReflect
    |> List.sum
    |> Num.toStr
    |> Ok

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
