interface Util
    exposes [toU64Unsafe, flatten]
    imports []

toU64Unsafe = \s ->
    when Str.toU64 s is
        Ok n -> n
        Err _ -> crash "invalid u8: \(s)"

flatten : List (List a) -> List a
flatten = \lst ->
    List.walk lst [] (\acc, x -> List.concat acc x)
