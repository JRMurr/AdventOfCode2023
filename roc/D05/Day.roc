interface D05.Day
    exposes [solution]
    imports [AoC, Util]

solution : AoC.Solution
solution = { day: 5, part1, part2 }

# This will probably be sad on full input..
# Parse the range str to a list of (src,dest)
parseRange : Str -> List (U64, U64)
parseRange = \s ->
    vals =
        Str.split s " "
        |> List.keepOks Str.toNat
    when vals is
        [dest, src, len] ->
            getRange = \start ->
                List.range { start: At start, end: Length len } |> List.map Num.toU64
            destRange = getRange dest
            srcRange = getRange src
            List.map2 srcRange destRange (\srcV, destV -> (srcV, destV))

        _ -> crash "bad range \(s)"

expect parseRange "50 98 2" == [(98, 50), (99, 51)]

# 50 98 2
# 52 50 48

parseFullMap : Str -> Dict U64 U64
parseFullMap = \s ->
    Str.split s "\n"
    |> List.map parseRange
    |> Util.flatten
    |> Dict.fromList

expect parseFullMap "50 98 2\n52 50 4" == Dict.fromList [(98, 50), (99, 51), (50, 52), (51, 53), (52, 54), (53, 55)]

# lookup in dict with v, if not found return v
getWithSelfDefault : Dict v v, v -> v
getWithSelfDefault = \dict, v ->
    Dict.get dict v
    |> Result.withDefault v

# start with an inital v, keep doing a pipe of lookups in the following dicts to get the end value
chainLookups : List (Dict v v), v -> v
chainLookups = \dicts, startVal ->
    List.walk
        dicts
        startVal
        (\curr, dict ->
            getWithSelfDefault dict curr
        )

expect chainLookups [Dict.fromList [(4, 50)], Dict.fromList [(51, 100)], Dict.fromList [(50, 2)]] 4 == 2

parseSeeds = \s ->
    Str.replace s "seeds: "
    |> Util.parseSpaceSepNums

parse = \s -> when Str.split in "\n\n" is
    [seeds, .. as maps] -> crash "TODO:"
    _ -> crash "bad input \(s)"



part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in -> 
    dbg (
        Str.split in "\n\n"
    )


    Err NotImplemented

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
