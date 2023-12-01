interface DXX.Day
    exposes [solution]
    imports [AoC]

solution : AoC.Solution
solution = { day: <UPDATE>, part1, part2 }

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \_ -> Err NotImplemented

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
