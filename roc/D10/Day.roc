interface D10.Day
    exposes [solution]
    imports [
        AoC,
        Util.{ unwrap, headTail },
        Coord.{ north, south, east, west },
        Array2D.{ Index, Array2D },
        Parser.Core.{ Parser, oneOf, const, skip, sepBy, oneOrMore, map, flatten },
        Parser.String.{ Utf8, parseStr, scalar },
    ]

solution : AoC.Solution
solution = { day: 10, part1, part2 }

Tile : [
    Vertical, # |
    Horizontal, # -
    NorthEast, # L : bottom left
    NorthWest, # J : bottom right
    SouthWest, # 7 : top right
    SouthEast, # F : top left
    Ground, # .
    Start, # S
]

tileToChar = \t ->
    when t is
        Vertical -> "|"
        Horizontal -> "-"
        NorthEast -> "L"
        NorthWest -> "J"
        SouthWest -> "7"
        SouthEast -> "F"
        Ground -> "."
        Start -> "S"

Grid : Array2D Tile

parseGrid : Str -> Grid
parseGrid = \s ->
    tile : Parser Utf8 Tile
    tile = oneOf [
        const Vertical |> skip (scalar '|'),
        const Horizontal |> skip (scalar '-'),
        const NorthEast |> skip (scalar 'L'),
        const NorthWest |> skip (scalar 'J'),
        const SouthWest |> skip (scalar '7'),
        const SouthEast |> skip (scalar 'F'),
        const Ground |> skip (scalar '.'),
        const Start |> skip (scalar 'S'),
    ]

    line = oneOrMore tile

    lines = line |> sepBy (scalar '\n')

    grid =
        lines
        |> map (\parsedLines -> (Array2D.fromExactLists parsedLines) |> Result.mapErr (\_ -> "InconsistentRowLengths"))
        |> flatten

    when parseStr grid s is
        Ok g -> g
        Err (ParsingFailure e) -> crash "ParsingFailure: \(e)"
        Err (ParsingIncomplete e) -> crash "ParsingIncomplete: \(e)"

findStart : Grid -> Index
findStart = \grid ->
    Array2D.findFirstIndex grid (\t -> t == Start) |> unwrap

# Given a postion, see what the possible neighbors to go to are
getPossibleNeighbors : Index, Grid -> List Index
getPossibleNeighbors = \currIdx, grid ->
    currTile = Array2D.get grid currIdx |> unwrap

    allowedAbove = [Start, Vertical, SouthEast, SouthWest]
    allowedBelow = [Start, Vertical, NorthEast, NorthWest]

    allowedLeft = [Start, Horizontal, NorthEast, SouthEast]
    allowedRight = [Start, Horizontal, NorthWest, SouthWest]

    DirAndAllowed : {
        dir : Index -> Result Index [OutOfBounds],
        allowedValues : List Tile,
    }

    getValidInDir : DirAndAllowed -> Result Index [InvalidIdx]
    getValidInDir = \dirAndAllowed ->
        { dir, allowedValues } = dirAndAllowed

        possibleNextIdx <- Result.try (dir currIdx |> Result.mapErr (\_ -> InvalidIdx))

        possible =
            Array2D.get grid possibleNextIdx
            |> Result.withDefault Ground

        isValid = List.contains allowedValues possible
        if isValid then Ok possibleNextIdx else Err InvalidIdx

    checkDirs : List DirAndAllowed -> List Index
    checkDirs = \checks ->
        List.keepOks checks getValidInDir

    checkAbove : DirAndAllowed
    checkAbove = { dir: \i -> north grid i, allowedValues: allowedAbove }

    checkBelow : DirAndAllowed
    checkBelow = { dir: \i -> south grid i, allowedValues: allowedBelow }

    checkLeft : DirAndAllowed
    checkLeft = { dir: \i -> west grid i, allowedValues: allowedLeft }

    checkRight : DirAndAllowed
    checkRight = { dir: \i -> east grid i, allowedValues: allowedRight }

    when currTile is
        Vertical -> checkDirs [checkAbove, checkBelow]
        Horizontal -> checkDirs [checkLeft, checkRight]
        NorthEast -> checkDirs [checkAbove, checkRight]
        NorthWest -> checkDirs [checkAbove, checkLeft]
        SouthWest -> checkDirs [checkBelow, checkLeft]
        SouthEast -> checkDirs [checkBelow, checkRight]
        Start ->
            # TODO: should only be two results?
            checkDirs [
                checkAbove,
                checkBelow,
                checkLeft,
                checkRight,
            ]

        Ground -> crash "you shouldnt be on a ground...."

# expect

QuededIdx : {
    idx : Index,
    history : List Index,
}

Visted : Set Index

# bfs until an idx we would queue is in visited so we know we hit the farthest possible point in the loop
findFarthest : Grid, List QuededIdx, Visted -> Nat
findFarthest = \grid, queue, visited ->
    (curr, queueNext) = headTail queue |> unwrap
    { idx, history } = curr

    possibleNext = getPossibleNeighbors idx grid

    filtedPossibleByHistory = possibleNext |> List.dropIf (\x -> List.contains history x)

    alreadySeen = Set.intersection visited (Set.fromList filtedPossibleByHistory)

    if Set.isEmpty alreadySeen then
        # keep going
        updatedHistory = history |> List.append idx
        toQueue = filtedPossibleByHistory |> List.map (\x -> { idx: x, history: updatedHistory })
        findFarthest grid (List.concat queueNext toQueue) (Set.insert visited idx)
    else
        # we are at the farthest point
        List.len history

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    grid = parseGrid in
    startIdx = findStart grid

    findFarthest grid [{ idx: startIdx, history: [] }] (Set.empty {})
    |> Num.toStr
    |> Ok

buildLoopTiles : Index, Grid, List QuededIdx -> List Index
buildLoopTiles = \startIdx, grid, queue ->
    (curr, queueNext) = headTail queue |> unwrap
    { idx, history } = curr

    possibleNext = getPossibleNeighbors idx grid

    filtedPossibleByLast =
        possibleNext
        |> List.dropIf
            (\x ->
                when curr.history is
                    [] -> Bool.false
                    [.., last] -> x == last
            )

    updatedHistory = history |> List.append idx

    if
        List.contains filtedPossibleByLast startIdx
    then
        updatedHistory
    else
        toQueue = filtedPossibleByLast |> List.map (\x -> { idx: x, history: updatedHistory })
        buildLoopTiles startIdx grid (List.concat queueNext toQueue)

getBoundingBox : List Index -> Result { min : Index, max : Index } [ListWasEmpty]
getBoundingBox = \lst ->
    xVals = List.map lst (.x)
    yVals = List.map lst (.y)

    minX <- List.min xVals |> Result.try
    maxX <- List.max xVals |> Result.try

    minY <- List.min yVals |> Result.try
    maxY <- List.max yVals |> Result.try

    Ok { min: { x: minX, y: minY }, max: { x: maxX, y: maxY } }

replaceStart : Grid, List Index, Index -> Grid
replaceStart = \grid, loop, startIdx ->
    getIndexinLoop = \idx -> List.findFirstIndex loop (\x -> x == idx)
    startIdxInLoop = getIndexinLoop startIdx |> unwrap

    loopLen = List.len loop
    allowedNeighborIdx =
        [
            startIdxInLoop + 1,
            startIdxInLoop + loopLen, # add loop len to not deal with negative nums
        ]
        |> List.map (\x -> x % loopLen)

    inAllowNeighbors = \neighbor ->
        when getIndexinLoop neighbor is
            Ok idx if List.contains allowedNeighborIdx idx -> Bool.true
            _ -> Bool.false

    dirs = [
        (North, north),
        (South, south),
        (East, east),
        (West, west),
    ]
    startNeighbors =
        List.keepOks
            dirs
            (\(tag, dir) ->
                when dir grid startIdx is
                    Ok neighbor if inAllowNeighbors neighbor -> Ok tag
                    _ -> Err "sad"
            )

    startLabel =
        when startNeighbors is
            [North, South] -> Vertical
            [East, West] -> Horizontal
            [North, East] -> NorthEast
            [North, West] -> NorthWest
            [South, East] -> SouthEast
            [South, West] -> SouthWest
            _ ->
                dbg
                    startNeighbors

                crash "bad neighbors"

    Array2D.set grid startIdx startLabel

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \in ->

    grid = parseGrid in
    startIdx = findStart grid

    loop = buildLoopTiles startIdx grid [{ idx: startIdx, history: [] }]

    { min, max } = getBoundingBox loop |> unwrap

    replacedStart = replaceStart grid loop startIdx

    # flipInOrOutOnLoop = \currOnLoop, nextOnLoop, inOrOut ->
    #     flipped = (if inOrOut == In then Out else In)
    #     # # Cant use Bool in pattern match???
    #     when inOrOut is
    #         Out if nextOnLoop -> In
    #         In if nextOnLoop -> Out
    #         _ -> inOrOut
    # _ if nextOnLoop -> Out
    # _ if currOnLoop && !nextOnLoop -> In
    # _ -> inOrOut

    # if
    #     (currOnLoop && !nextOnLoop)

    # then
    #     (if inOrOut == In then Out else In)
    # else
    #     inOrOut

    # followDirUntil : Index, (Index -> Result Index [OutOfBounds]), (Index -> Bool) -> Result Index [NotFound]
    # followDirUntil = \currIdx, dir, pred ->
    #     if pred currIdx then
    #         Ok currIdx
    #     else
    #         when dir currIdx is
    #             Ok newIdx -> followDirUntil newIdx dir pred
    #             Err _ -> Err NotFound

    # # followDirUntilLoop = \start, dir ->
    # #     followDirUntil start dir (\c -> List.contains loop c)

    # followDirUntilNotLoop = \start, dir ->
    #     followDirUntil start dir (\c -> !(List.contains loop c))

    flipInOrOutOnLoop = \shouldFlip, inOrOut ->
        flipped = (if inOrOut == In then Out else In)
        if shouldFlip then flipped else inOrOut

    walkGridLine : Index, (Index -> Result Index [OutOfBounds]), [In, Out], Set Index -> Set Index
    walkGridLine = \currIdx, dir, inOrOut, acc ->
        onLoop = List.contains loop currIdx

        shouldFlip =
            when (Array2D.get replacedStart currIdx |> unwrap) is
                Horizontal -> Bool.true
                Vertical -> Bool.true
                _ -> Bool.false

        newAcc = if inOrOut == In && !onLoop then Set.insert acc currIdx else acc

        when dir currIdx is
            Ok newIdx ->
                nextOnLoop = List.contains loop newIdx
                newInOrOut = flipInOrOutOnLoop shouldFlip inOrOut

                walkGridLine newIdx dir newInOrOut newAcc

            Err _ -> newAcc

    walkGridLines : List Index, (Index -> Result Index [OutOfBounds]) -> Set Index
    walkGridLines = \startIdxs, dir ->
        List.walk
            startIdxs
            (Set.empty {})
            (\acc, start ->
                walkGridLine start dir Out acc
            )

    checkedRows =
        List.range { start: At min.x, end: At max.x }
        |> List.map (\x -> { x, y: 0 })
        |> walkGridLines (\i -> east grid i)
    checkedCols =
        List.range { start: At min.y, end: At max.y }
        |> List.map (\y -> { y, x: 0 })
        |> walkGridLines (\i -> south grid i)

    intersection = Set.intersection checkedRows checkedCols

    drawnGrid = Util.drawArray
        grid
        (\tile, _ ->
            tileToChar tile
        )

    drawnGridWithMarked = Util.drawArray
        grid
        (\tile, idx ->
            if Set.contains checkedRows idx then
                "I"
            else
                tileToChar tile
        )

    dbg
        "\n"

    dbg
        drawnGrid

    dbg
        "\n"

    dbg
        drawnGridWithMarked

    Set.len intersection
    |> Num.toStr
    |> Ok
