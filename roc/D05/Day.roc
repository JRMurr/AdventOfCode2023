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

parseFullMap : Str -> RangeMap
parseFullMap = \s ->
    Str.split s "\n"
    |> List.map parseRange

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

# parseSeeds = \s ->
#     Str.replace s "seeds: "
#     |> Util.parseSpaceSepNums

parse = \s ->
    when Str.split s "\n\n" is
        [seeds, .. as maps] -> crash "TODO:"
        _ -> crash "bad input \(s)"

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    dbg
        Str.split in "\n\n"

    Err NotImplemented

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
