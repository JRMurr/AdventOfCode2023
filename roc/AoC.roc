interface AoC
    exposes [Solution, PartRes]
    imports []
# would like this to live in App but causes cirular deps when days import the solution type

PartRes : Result Str [NotImplemented, Error Str]

Solution : {
    day : U8,
    part1 : Str -> PartRes,
    part2 : Str -> PartRes,
}
