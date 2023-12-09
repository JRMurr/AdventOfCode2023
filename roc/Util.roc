interface Util
    exposes [toU64Unsafe, flatten, toNatUnsafe, parseSpaceSepNums, tuplify, countElems, unwrap]
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

parseSpaceSepNums : Str -> List U64
parseSpaceSepNums = \s ->
    Str.split s " "
    |> List.keepOks Str.toU64

tuplify : List a -> (a, a)
tuplify = \lst ->
    when lst is
        [x, y] -> (x, y)
        _ -> crash "bad list to tuplify"

countElems : List a -> Dict a U64 where a implements Hash & Eq
countElems = \lst ->
    List.walk
        lst
        (Dict.empty {})
        (\acc, x ->
            Dict.update
                acc
                x
                (\v ->
                    when v is
                        Present c -> Present (c + 1)
                        Missing -> Present 1
                )
        )

unwrap : Result a * -> a
unwrap = \res ->
    when res is
        Ok x -> x
        Err _ -> crash "bad unwrap"
