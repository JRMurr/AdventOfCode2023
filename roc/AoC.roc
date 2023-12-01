interface AoC
    exposes [Solution]
    imports []
# would like this to live in App but causes cirular deps when days import the solution type

Solution : {
    day : U8,
    part1 : Str -> Result Str [NotImplemented, Error Str],
    part2 : Str -> Result Str [NotImplemented, Error Str],
}
