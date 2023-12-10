interface D10.Day
    exposes [solution]
    imports [
        AoC,
        Util.{ unwrap, headTail },
        Coord,
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

    allowedAbove = [Vertical, SouthEast, SouthWest]
    allowedBelow = [Vertical, NorthEast, NorthWest]

    allowedLeft = [Horizontal, NorthEast, SouthEast]
    allowedRight = [Horizontal, NorthWest, SouthWest]

    getValidInDir = \(dir, allowedVals) ->
        possibleNextIdx = dir currIdx
        dbg possibleNextIdx
        possible = Array2D.get grid possibleNextIdx |> Result.withDefault Ground
        dbg possible
        isValid = List.contains allowedVals possible
        if isValid then Ok possibleNextIdx else Err InvalidIdx

    checkAbove = (Coord.north, allowedAbove)
    checkBelow = (Coord.south, allowedBelow)
    checkLeft = (Coord.west, allowedLeft)
    checkRight = (Coord.east, allowedRight)

    checkDirs = \checks ->
        checks |> List.keepOks getValidInDir

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

    dbg
        possibleNext

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

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
