interface Util
    exposes [
        toU64Unsafe,
        flatten,
        toNatUnsafe,
        parseSpaceSepNums,
        tuplify,
        tuplify3,
        countElems,
        unwrap,
        dbge,
        mapAdjacent,
        mapLines,
        headTail,
        drawArray,
    ]
    imports [
        Array2D.{ Index, Array2D },
    ]

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

tuplify3 : List a -> (a, a, a)
tuplify3 = \lst ->
    when lst is
        [x, y, z] -> (x, y, z)
        _ -> crash "bad list to tuplify3"

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

dbge = \x ->
    dbg
        x

    x

mapAdjacent : List a, (a, a -> c) -> List c
mapAdjacent = \list, f ->
    List.map2 list (List.dropFirst list 1) f

mapLines : Str, (Str -> a) -> List a
mapLines = \in, mapper ->
    Str.split in "\n"
    |> List.map mapper

headTail : List a -> Result (a, List a) [EmptyList]
headTail = \lst ->
    when lst is
        [head, .. as tail] -> Ok (head, tail)
        _ -> Err EmptyList

drawArray : Array2D a, (a, Index -> Str) -> Str
drawArray = \arr, mapper ->
    Array2D.walk
        arr
        ["\n"]
        { direction: Forwards, orientation: Rows }
        (\acc, elem, idx ->

            mappedElem = mapper elem idx

            withNewLine = if Array2D.isRowEnd arr idx then "\(mappedElem)\n" else mappedElem

            List.append acc withNewLine
        )
    |> Str.joinWith ""
