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

shoeLace : List Index -> I64
shoeLace = \lst ->
    mapped = List.map lst (\{ x, y } -> (x |> Num.toI64, y |> Num.toI64))

    go : List (I64, I64) -> I64
    go = \updated ->
        when updated is
            [(x1, y1), (x2, y2), .. as rest] ->
                (y1 + y2) * (x2 - x1) + (List.prepend rest (x2, y2) |> go)

            _ -> 0

    go mapped

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \in ->

    grid = parseGrid in
    startIdx = findStart grid

    loop = buildLoopTiles startIdx grid [{ idx: startIdx, history: [] }]

    dbg
        shoeLace loop

    # https://www.reddit.com/r/adventofcode/comments/18evyu9/2023_day_10_solutions/kcqmhwk/
    ((Num.abs (shoeLace loop) - (List.len loop |> Num.toI64) + 3))
    // 2
    |> Num.toStr
    |> Ok
