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

relfectAxis : [Row, Col] -> (Index, Nat, Nat -> Index)
relfectAxis = \axis -> \idx, flipIdx, dim ->
        val = if axis == Row then idx.y else idx.x
        isEven = dim % 2 == 0
        flippingMid = isEven && flipIdx == (dim // 2)
        offset = if flippingMid then 0 else 1

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

reflectOverRow : Index, Nat, Nat -> Index
reflectOverRow = relfectAxis Row

expect reflectOverRow { x: 0, y: 4 } 4 9 == { x: 0, y: 5 }
expect reflectOverRow { x: 0, y: 5 } 4 9 == { x: 0, y: 4 }
expect reflectOverRow { x: 0, y: 1 } 4 9 == { x: 0, y: 8 }
expect reflectOverRow { x: 0, y: 0 } 4 9 == { x: 0, y: 9 }

expect reflectOverRow { x: 0, y: 6 } 7 14 == { x: 0, y: 8 }
expect reflectOverRow { x: 0, y: 7 } 7 14 == { x: 0, y: 7 }
expect reflectOverRow { x: 0, y: 8 } 7 14 == { x: 0, y: 6 }

reflectOverCol : Index, Nat, Nat -> Index
reflectOverCol = relfectAxis Col

expect reflectOverCol { y: 0, x: 4 } 4 9 == { y: 0, x: 5 }

validFlip : Pattern, Tile, Index -> Bool
validFlip = \pattern, ogVal, flippedIdx ->
    when Array2D.get pattern flippedIdx is
        Ok vFlip -> ogVal == vFlip
        Err _ -> Bool.true

findReflect : Pattern -> Result Nat [NotFound]
findReflect = \pattern ->
    { dimX: numCols, dimY: numRows } = Array2D.shape pattern

    dbg { dimX: numCols, dimY: numRows } 

    checkFlip = \flipper, flipIdx, dim ->
        Array2D.walkUntil
            pattern
            Bool.true
            { direction: Forwards }
            (\_, val, idx ->
                flipped = flipper idx flipIdx dim
                if validFlip pattern val flipped then
                    Continue Bool.true
                else
                    dbg
                        { val, idx, flipped, flipIdx }
                    Break Bool.false
            )

    findFlip = \flipper, dim ->
        List.range { start: At 0, end: Before dim }
        |> List.walkUntil
            (Err NotFound)
            (\acc, flipIdx ->
                dbg flipIdx
                if checkFlip flipper flipIdx dim then Break (Ok flipIdx) else Continue (acc)
            )

    # dbg {numCols, numRows}

    # TODO: only need to walk up to the flip idx
    validColFlip = findFlip reflectOverCol numCols
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
    dbg validColFlip
    if validColFlip |> Result.isOk then
        Result.map validColFlip (\possibleColFlip -> (possibleColFlip + 1) * 100)
    else
        validRowFlip = findFlip reflectOverRow numRows
        dbg validRowFlip
        Result.map validRowFlip (\possibleRowFlip -> (possibleRowFlip + 1))

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
