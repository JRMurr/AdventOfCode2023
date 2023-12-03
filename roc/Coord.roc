interface Coord
    exposes [
        Coord,
        # dirs
        north,
        south,
        east,
        west,
        northEast,
        northWest,
        southEast,
        southWest,

        # all dirs
        neighbors,
    ]
    imports [
        Array2D.{Index}
    ]

# Coord : { x : Nat, y : Nat }{}

Coord : Index

north : Coord -> Coord
north = \c -> { x: c.x, y: c.y - 1 }

south : Coord -> Coord
south = \c -> { x: c.x, y: c.y + 1 }

east : Coord -> Coord
east = \c -> { x: c.x + 1, y: c.y }

west : Coord -> Coord
west = \c -> { x: c.x - 1, y: c.y }

northEast : Coord -> Coord
northEast = \c ->
    north c |> east

northWest : Coord -> Coord
northWest = \c ->
    north c |> west

southWest : Coord -> Coord
southWest = \c ->
    south c |> west

southEast : Coord -> Coord
southEast = \c ->
    south c |> east

#
# Get all Coords around this one
#
neighbors : Coord -> List Coord
neighbors = \c ->
    List.map
        [
            north,
            south,
            east,
            west,
            northEast,
            northWest,
            southEast,
            southWest,
        ]
        (\f -> f c)
