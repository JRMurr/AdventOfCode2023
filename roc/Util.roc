interface Util
    exposes [toU64Unsafe, flatten, toNatUnsafe, parseSpaceSepNums]
    imports []

toU64Unsafe = \s ->
    when Str.toU64 s is
        Ok n -> n
        Err _ -> crash "invalid U64: \(s)"

toNatUnsafe = \s ->
    when Str.toNat s is
        Ok n -> n
        Err _ -> crash "invalid nat: \(s)"

flatten : List (List a) -> List a
flatten = \lst ->
    List.walk lst [] (\acc, x -> List.concat acc x)

parseSpaceSepNums : Str -> Set U64
parseSpaceSepNums = \s ->
    Str.split s " "
    |> List.keepOks Str.toU64
    |> Set.fromList
