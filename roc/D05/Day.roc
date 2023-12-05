interface D05.Day
    exposes [solution]
    imports [AoC, Util]

solution : AoC.Solution
solution = { day: 5, part1, part2 }

Range : {
    src : U64,
    dest : U64,
    len : U64,
}

parseRange : Str -> Range
parseRange = \s ->
    vals =
        Str.split s " "
        |> List.keepOks Str.toU64
    when vals is
        [dest, src, len] ->
            { src, dest, len }

        _ -> crash "bad range \(s)"

# # This will probably be sad on full input..
# # Parse the range str to a list of (src,dest)
# parseRange : Str -> List (U64, U64)
# parseRange = \s ->
#     vals =
#         Str.split s " "
#         |> List.keepOks Str.toNat
#     when vals is
#         [dest, src, len] ->
#             getRange = \start ->
#                 List.range { start: At start, end: Length len } |> List.map Num.toU64
#             destRange = getRange dest
#             srcRange = getRange src
#             List.map2 srcRange destRange (\srcV, destV -> (srcV, destV))

#         _ -> crash "bad range \(s)"

# expect parseRange "50 98 2" == [(98, 50), (99, 51)]

# 50 98 2
# 52 50 48

RangeMap : List Range

parseFullMap : Str -> (Str, RangeMap)
parseFullMap = \s ->
    dbg s
    when Str.split s "\n" is
        [name, .. as ranges] -> (name, List.map ranges parseRange)
        _ -> crash "bad map \(s)"

# expect parseFullMap "50 98 2\n52 50 48" == [{src: 98, dest: 50, len 2}, {src: 50, dest: 52, len 48}]

getMappingInRange : Range, U64 -> Result U64 [NoMapping]
getMappingInRange = \range, v ->
    { src, len, dest } = range
    if (src <= v && v < src + len) then
        offset = v - src
        Ok (dest + offset)
    else
        Err NoMapping

getMapping : RangeMap, U64 -> U64
getMapping = \ranges, v ->
    List.walkUntil
        ranges
        v
        (\default, range ->
            when getMappingInRange range v is
                Ok x -> Break x
                _ -> Continue default
        )

# start with an inital v, keep doing a pipe of lookups in the following maps to get the end value
chainLookups : List RangeMap, U64 -> U64
chainLookups = \maps, startVal ->
    List.walk
        maps
        startVal
        (\curr, map ->
            getMapping map curr
        )

# expect chainLookups [Dict.fromList [(4, 50)], Dict.fromList [(51, 100)], Dict.fromList [(50, 2)]] 4 == 2

parseSeeds = \s ->
    Str.replaceEach s "seeds: " ""
    |> Util.parseSpaceSepNums
    |> Set.toList

Input : {
    seeds : List U64,
    maps : List (Str, RangeMap),
}

parse : Str -> Input
parse = \s ->
    when Str.split s "\n\n" is
        [seeds, .. as maps] -> { seeds: parseSeeds seeds, maps: List.map maps parseFullMap }
        _ -> crash "bad input \(s)"

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    { seeds, maps: mapsWithName } = in |> Str.trim |> parse

    maps = List.map mapsWithName (.1)

    List.map seeds (\seed -> chainLookups maps seed)
    |> List.min
    |> Result.mapErr (\_ -> Error "impossible")
    |> Result.map (Num.toStr)

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
