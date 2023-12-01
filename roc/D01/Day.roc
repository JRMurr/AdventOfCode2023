interface D01.Day
    exposes [solution]
    imports [AoC]

solution : AoC.Solution
solution = { day: 1, part1, part2 }

# listToNum : List U64 -> U64
listToNum = \lst ->
    when lst is
        [x, y] -> x * 10 + y
        _ -> crash "invalud list"

getDigits : Str -> Int Natural
getDigits = \s ->
    Str.graphemes s
    |> List.keepOks (Str.toNat)
    |> List.takeFirst 2
    |> listToNum

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    Str.split in "\n"
    |> List.map getDigits
    |> List.sum
    |> Num.toStr
    |> Ok

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
