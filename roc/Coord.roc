interface Coord
    exposes [
        Coord,

        # dirs
        east,
        west,
        north,
        south,
        northUnsafe,
        southUnsafe,
        eastUnsafe,
        westUnsafe,
        northEastUnsafe,
        northWestUnsafe,
        southEastUnsafe,
        southWestUnsafe,

        # all dirs
        getNeighbors,
    ]
    imports [
        Array2D.{ Array2D, Index, isRowEnd, isColEnd, isRowStart, isColStart, shape },
    ]

# Coord : { x : Nat, y : Nat }{}

east : Array2D *, Index -> Result Index [OutOfBounds]
east = \array, index ->
    if isRowEnd array index then
        if isColEnd array index then
            Err OutOfBounds
        else
            Ok { x: index.x + 1, y: 0 }
    else
        Ok { x: index.x, y: index.y + 1 }

west : Array2D *, Index -> Result Index [OutOfBounds]
west = \array, index ->
    if isRowStart index then
        if isColStart index then
            Err OutOfBounds
        else
            Ok { x: index.x - 1, y: (shape array).dimY - 1 }
    else
        Ok { x: index.x, y: index.y - 1 }

south : Array2D *, Index -> Result Index [OutOfBounds]
south = \array, index ->
    if isColEnd array index then
        if isRowEnd array index then
            Err OutOfBounds
        else
            Ok { x: 0, y: index.y + 1 }
    else
        Ok { x: index.x + 1, y: index.y }

north : Array2D *, Index -> Result Index [OutOfBounds]
north = \array, index ->
    if isColStart index then
        if isRowStart index then
            Err OutOfBounds
        else
            Ok { x: (shape array).dimX - 1, y: index.y - 1 }
    else
        Ok { x: index.x - 1, y: index.y }

Coord : Index

southUnsafe : Coord -> Coord
southUnsafe = \c -> { x: c.x + 1, y: c.y }

northUnsafe : Coord -> Coord
northUnsafe = \c -> { x: c.x - 1, y: c.y }

westUnsafe : Coord -> Coord
westUnsafe = \c ->
    { x: c.x, y: c.y - 1 }

eastUnsafe : Coord -> Coord
eastUnsafe = \c -> { x: c.x, y: c.y + 1 }

northEastUnsafe : Coord -> Coord
northEastUnsafe = \c ->
    northUnsafe c |> eastUnsafe

northWestUnsafe : Coord -> Coord
northWestUnsafe = \c ->
    northUnsafe c |> westUnsafe

southWestUnsafe : Coord -> Coord
southWestUnsafe = \c ->
    southUnsafe c |> westUnsafe

southEastUnsafe : Coord -> Coord
southEastUnsafe = \c ->
    southUnsafe c |> eastUnsafe

#
# Get all Coords around this one
#
getNeighbors : Coord -> List Coord
getNeighbors = \c ->
    List.map
        [
            northUnsafe,
            southUnsafe,
            eastUnsafe,
            westUnsafe,
            northEastUnsafe,
            northWestUnsafe,
            southEastUnsafe,
            southWestUnsafe,
        ]
        (\f -> f c)

expect
    getNeighbors { x: 1, y: 1 }
    == [
        { x: 0, y: 1 }, # u
        { x: 2, y: 1 }, # d
        { x: 1, y: 2 }, # r
        { x: 1, y: 0 }, # l

        { x: 0, y: 2 }, # uR
        { x: 0, y: 0 }, # ul
        { x: 2, y: 2 }, # dr
        { x: 2, y: 0 }, # dl
    ]
