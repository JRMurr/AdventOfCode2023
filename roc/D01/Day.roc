interface D01.Day
    exposes [solution]
    imports [AoC]

solution : AoC.Solution
solution = { day: 1, part1, part2 }

# getDigits : Str -> Int Natural
getDigits = \s ->
    lst =
        Str.graphemes s
        |> List.keepOks (Str.toNat)

    when (List.first lst, List.last lst) is
        (Ok x, Ok y) -> (x * 10) + y
        _ -> crash "invalid list: \(s)"

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    Str.split in "\n"
    |> List.dropIf (\s -> s == "")
    |> List.map getDigits
    |> List.sum
    |> Num.toStr
    |> Ok

strToNum : Dict Str Nat
strToNum =
    Dict.empty {}
    |> Dict.insert "one" 1
    |> Dict.insert "two" 2
    |> Dict.insert "three" 3
    |> Dict.insert "four" 4
    |> Dict.insert "five" 5
    |> Dict.insert "six" 6
    |> Dict.insert "seven" 7
    |> Dict.insert "eight" 8
    |> Dict.insert "nine" 9

# 3, 4, or 5 long

lstIsNum : List Str -> Result Nat [NotNum]
lstIsNum = \lst ->
    when lst is
        [x] -> Str.toNat x |> Result.mapErr \_ -> NotNum
        _ ->
            joined = Str.joinWith lst ""

            when Dict.get strToNum joined is
                Ok val -> Ok val
                Err _ -> Err NotNum

getDigitsP2 = \s ->
    lst =
        Str.graphemes s
    range = List.range { start: At 0, end: At ((List.len lst) - 1) }
    nums = List.joinMap
        range
        (\i ->
            subListLens = [1, 3, 4, 5]

            List.keepOks
                subListLens
                (\len ->
                    lstIsNum (List.sublist lst { start: i, len })
                )
        )

    when (List.first nums, List.last nums) is
        (Ok x, Ok y) -> (x * 10) + y
        _ -> crash "invalid list: \(s)"

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \in ->
    Str.split in "\n"
    |> List.dropIf (\s -> s == "")
    |> List.map getDigitsP2
    |> List.sum
    |> Num.toStr
    |> Ok
