interface D11.Day
    exposes [solution]
    imports [
        AoC,
        Util,
        Coord.{ distance },
        Array2D.{ Index, Array2D },
        Parser.Core.{ Parser, oneOf, const, skip },
        Parser.String.{ Utf8, scalar },
    ]

solution : AoC.Solution
solution = { day: 11, part1, part2 }

Tile : [Empty, Galaxy]

Grid : Array2D Tile

parseGrid : Str -> Grid
parseGrid = \s ->
    tile = oneOf [
        const Empty |> skip (scalar '.'),
        const Galaxy |> skip (scalar '#'),
    ]

    when Util.parse2D s tile is
        Ok g -> g
        Err e -> crash e

findGalaxys : Grid -> Set Index
findGalaxys = \grid ->
    Array2D.walk
        grid
        (Set.empty {})
        { direction: Forwards }
        (\acc, tile, idx ->
            if tile == Galaxy then Set.insert acc idx else acc
        )

expandGrid : Grid -> Grid
expandGrid = \grid ->
    galaxyIdxs = findGalaxys grid
    { dimX, dimY } = Array2D.shape grid

    possibleXs = List.range { start: At 0, end: Before dimX } |> Set.fromList
    possibleYs = List.range { start: At 0, end: Before dimY } |> Set.fromList

    seenXs = Set.map galaxyIdxs .x
    seenYs = Set.map galaxyIdxs .y

    missingXs = Set.difference possibleXs seenXs
    missingYs = Set.difference possibleYs seenYs

    dbg
        { missingXs, missingYs }

    numLessThan : Set Nat, Nat -> Nat
    numLessThan = \set, val ->
        Set.keepIf set (\x -> x < val)
        |> Set.len

    # for each galaxy
    # new x = old x + <number of missingXs < old x>
    # same for y
    newGalaxyIdxs = Set.map
        galaxyIdxs
        (\gIdx ->
            { x: oldX, y: oldY } = gIdx
            x = oldX + (numLessThan missingXs oldX)
            y = oldY + (numLessThan missingYs oldY)

            { x, y }
        )

    newShape = { dimX: dimX + Set.len missingXs, dimY: dimY + Set.len missingYs }

    Array2D.init
        newShape
        (\idx ->
            if Set.contains newGalaxyIdxs idx then Galaxy else Empty
        )

getDistances : Grid -> Nat
getDistances = \grid ->
    galaxyIdxs = findGalaxys grid
    pairedGalaxyIdxs = Util.cartProdUnique (Set.toList galaxyIdxs)

    pairedGalaxyIdxs
    |> List.map (\(a, b) -> distance a b)
    |> List.sum

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->

    grid =
        parseGrid in
        |> expandGrid
    # dbg
    #     (Util.drawArray grid (\t, _ -> if t == Galaxy then "#" else "."))

    # dbg
    #     (Util.drawArray (expandGrid grid) (\t, _ -> if t == Galaxy then "#" else "."))
    getDistances grid
    |> Num.toStr
    |> Ok
# Err NotImplemented

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented