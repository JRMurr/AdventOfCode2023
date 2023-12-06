interface D06.Day
    exposes [solution]
    imports [AoC, Util]

solution : AoC.Solution
solution = { day: 6, part1, part2 }

Race : { time : U64, distance : U64 }

parse : Str -> List Race
parse = \s ->
    parseLine = \l ->
        Str.split l ":"
        |> Util.tuplify
        |> .1
        |> Util.parseSpaceSepNums

    (times, dists) =
        s
        |> Str.trim
        |> Str.split "\n"
        |> List.map parseLine
        |> Util.tuplify

    List.map2 times dists (\time, distance -> { time, distance })

expect parse "Time:      7  15   30\nDistance:  9  40  200\n" == [{ time: 7, distance: 9 }, { time: 15, distance: 40 }, { time: 30, distance: 200 }]

ChargeRange : { min : U64, max : U64 }

getDistWithCharge : { chargeTime : U64, totalTime : U64 } -> U64
getDistWithCharge = \x ->
    if x.chargeTime >= x.totalTime then
        0
    else
        speed = x.chargeTime
        remainingTime = x.totalTime - x.chargeTime
        remainingTime * speed

# getMaxChargeTime : Race -> U64
# getMaxChargeTime = \race ->

getChargeRange : Race -> ChargeRange
getChargeRange = \race ->
    { time, distance } = race

    walker : U64, U64 -> [Break U64, Continue U64]
    walker = \acc, chargeTime ->
        if
            getDistWithCharge { chargeTime, totalTime: time } > distance
        then
            Break chargeTime
        else
            Continue acc

    times = List.range { start: At 1, end: Before time } |> List.map Num.toU64

    min = List.walkUntil times 0 walker
    max = List.walkBackwardsUntil times 0 walker
    { min, max }

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    in
    |> parse
    |> List.map getChargeRange
    |> List.map (\x -> x.max - x.min + 1)
    |> List.product
    |> Num.toStr
    |> Ok

parseP2 : Str -> Race
parseP2 = \s ->
    parseLine = \l ->
            Str.split l ":"
            |> Util.tuplify
            |> .1
            |> Str.replaceEach " " "" 
            |> Util.parseSpaceSepNums
    (time, distance) =
        s
        |> Str.trim
        |> Str.split "\n"
        |> List.map parseLine
        |> Util.tuplify
    {time, ditance}

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \in -> 
    dbg in
        |> parseP2

    Err NotImplemented
