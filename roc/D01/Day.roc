interface D01.Day
    exposes [solution]
    imports [AoC]

solution : AoC.Solution
solution = { day: 1, part1, part2 }

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \_ -> Err NotImplemented

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
