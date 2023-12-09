interface D09.Day
    exposes [solution]
    imports [AoC, Util]

solution : AoC.Solution
solution = { day: 9, part1, part2 }

Value : I64
Values : List I64

parseLine : Str -> Values
parseLine = \s ->
    Str.split s " "
    |> List.keepOks Str.toI64

expect parseLine "0 3 6 9 12 15" == [0, 3, 6, 9, 12, 15]

getDiffs : Values -> Values
getDiffs = \lst ->
    Util.mapAdjacent
        lst
        (\x, y ->
            y - x
        )

expect getDiffs [0, 3, 6, 9, 12, 15] == [3, 3, 3, 3, 3]

getNextValue : Values -> Value
getNextValue = \lst ->
    diffs = getDiffs lst
    toAdd = (
        if List.len diffs == 0 || List.all diffs (\x -> x == 0) then
            0
        else
            getNextValue diffs
    )
    last = List.last lst |> Util.unwrap
    last + toAdd

expect getNextValue [0, 3, 6, 9, 12, 15] == 18

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in -> in
    |> Util.mapLines parseLine
    |> List.map getNextValue
    |> List.sum
    |> Num.toStr
    |> Ok

getFirstValue : Values -> Value
getFirstValue = \lst ->
    diffs = getDiffs lst
    toSub = (
        if List.len diffs == 0 || List.all diffs (\x -> x == 0) then
            0
        else
            getFirstValue diffs
    )
    first = List.first lst |> Util.unwrap
    first - toSub

expect getFirstValue (parseLine "10 13 16 21 30 45") == 5

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \in -> in
    |> Util.mapLines parseLine
    |> List.map getFirstValue
    |> List.sum
    |> Num.toStr
    |> Ok
