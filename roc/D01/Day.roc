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

strToNum : Dict Str Str
strToNum =
    Dict.empty {}
    |> Dict.insert "one" "1"
    |> Dict.insert "two" "2"
    |> Dict.insert "three" "3"
    |> Dict.insert "four" "4"
    |> Dict.insert "five" "5"
    |> Dict.insert "six" "6"
    |> Dict.insert "seven" "7"
    |> Dict.insert "eight" "8"
    |> Dict.insert "nine" "9"

getDigitsP2 = \s ->
    # replace each occurance of the word num with `dight<word>`
    # hopefully covers the case when two spellings overlap?
    # might not...
    # EDIT: does not..
    newStr = Dict.walk strToNum s (\acc, word, val -> Str.replaceEach acc word "\(val)\(word)")
    getDigits newStr

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \in ->
    Str.split in "\n"
    |> List.dropIf (\s -> s == "")
    |> List.map getDigitsP2
    |> List.sum
    |> Num.toStr
    |> Ok
