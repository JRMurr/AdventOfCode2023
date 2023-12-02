interface D02.Day
    exposes [solution]
    imports [AoC, Util]

solution : AoC.Solution
solution = { day: 2, part1, part2 }

Color : [Blue, Red, Green]

parseColor : Str -> Color
parseColor = \s ->
    when s is
        "red" -> Red
        "blue" -> Blue
        "green" -> Green
        _ -> crash "invalid color: \(s)"

CubeCount : { color : Color, count : U64 }

parseCubeCount : Str -> CubeCount
parseCubeCount = \s ->
    when Str.split s " " is
        [numStr, cStr] -> { count: Util.toU64Unsafe numStr, color: parseColor cStr }
        _ -> crash "invalid cube count: \(s)"

SubSet : List CubeCount

# Ex: 1 red, 2 green, 6 blue
parseSubSet : Str -> SubSet
parseSubSet = \s ->
    Str.split s ", "
    |> List.map parseCubeCount

parseSubsets : Str -> List SubSet
parseSubsets = \s ->
    Str.split s "; "
    |> List.map parseSubSet

Game : { id : U64, subsets : List SubSet }

parseGameId : Str -> U64
parseGameId = \s ->
    removedPrefix = Str.replaceFirst s "Game " ""
    Util.toU64Unsafe removedPrefix

parseGame : Str -> Result Game [Error Str]
parseGame = \s ->
    when Str.splitFirst s ": " is
        Ok { before: gameStr, after: subSets } -> Ok ({ id: parseGameId gameStr, subsets: parseSubsets subSets })
        Err _ -> Err (Error "bad game \(s)")

BagConfg : List CubeCount

cubCountGreater : CubeCount, CubeCount -> Bool
cubCountGreater = \seen, bagColorCount ->
    if seen.color != bagColorCount.color then
        Bool.false
    else
        seen.count > bagColorCount.count

isGameValid : Game, BagConfg -> Bool
isGameValid = \g, bagConfig ->
    { subsets } = g

    allCounts = Util.flatten subsets

    bagInvalid = \cubeCount -> List.any bagConfig (\bagColorCount -> cubCountGreater cubeCount bagColorCount)

    List.any allCounts bagInvalid |> Bool.not

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->

    bagConf : BagConfg
    bagConf = [
        { color: Red, count: 12u64 },
        { color: Green, count: 13u64 }, # TODO: no good compiler error when i missed a `:` after count here
        { color: Blue, count: 14u64 },
    ]

    # vals =
    Str.split in "\n"
    |> List.keepOks parseGame
    |> List.keepIf (\g -> isGameValid g bagConf)
    |> List.map (.id)
    |> List.sum
    |> Num.toStr
    |> Ok

MinConfig : { red : U64, green : U64, blue : U64 }

# addToConfig

calcMinCube : Game -> MinConfig
calcMinCube = \g ->
    { subsets } = g

    allCounts = Util.flatten subsets

    reducer : MinConfig, CubeCount -> MinConfig
    reducer = \acc, cubeCount ->
        { red, green, blue } = acc
        when cubeCount is
            { color: Red, count } -> { red: Num.max red count, green, blue }
            { color: Green, count } -> { red, green: Num.max green count, blue }
            { color: Blue, count } -> { red, green, blue: Num.max blue count }

    List.walk allCounts { red: 0, green: 0, blue: 0 } reducer

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \in ->
    Str.split in "\n"
    |> List.keepOks parseGame
    |> List.map calcMinCube
    |> List.map (\c -> c.red * c.green * c.blue)
    |> List.sum
    |> Num.toStr
    |> Ok
