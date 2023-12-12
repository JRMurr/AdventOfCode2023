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

# expand grid and get back the new galaxyIndexs
expandGrid : Grid, Nat -> Set Index
expandGrid = \grid, growthFactor ->
    replaceMentMul = if growthFactor == 0 then 0 else growthFactor - 1
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
        |> Num.mul replaceMentMul

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
    newGalaxyIdxs

getDistances : Set Index -> Nat
getDistances = \galaxyIdxs ->
    pairedGalaxyIdxs = Util.cartProdUnique (Set.toList galaxyIdxs)

    pairedGalaxyIdxs
    |> List.map (\(a, b) -> distance a b)
    |> List.sum

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->

    galaxyIdxs =
        parseGrid in
        |> expandGrid 2
    getDistances galaxyIdxs
    |> Num.toStr
    |> Ok

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \in ->
    galaxyIdxs =
        parseGrid in
        |> expandGrid 1_000_000

    getDistances galaxyIdxs
    |> Num.toStr
    |> Ok
