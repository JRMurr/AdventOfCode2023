interface D01.Day
    exposes [solution]
    imports [AoC]

solution : AoC.Solution
solution = { day: 1, part1, part2 }

# getDigits : Str -> Int Natural
getDigits = \s ->
    lst = Str.graphemes s
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

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
