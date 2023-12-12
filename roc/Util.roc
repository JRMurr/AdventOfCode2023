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
        parse2D,
        withIndex,
        cartProdUnique,
    ]
    imports [
        Array2D.{ Index, Array2D },
        Parser.Core.{ Parser, oneOrMore, sepBy, map },
        Parser.String.{ Utf8, parseStr, scalar },
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

# Parse a map of tiles into a 2d array with the given tile parser
parse2D : Str, Parser Utf8 tile -> Result (Array2D tile) Str
parse2D = \s, tileParser ->
    line : Parser Utf8 (List tile)
    line = oneOrMore tileParser

    lines : Parser Utf8 (List (List tile))
    lines = line |> sepBy (scalar '\n')

    grid : Parser Utf8 (Array2D tile)
    grid =
        lines
        |> map (\parsedLines -> (Array2D.fromExactLists parsedLines) |> Result.mapErr (\_ -> "InconsistentRowLengths"))
        |> Parser.Core.flatten

    when parseStr grid (Str.trim s) is
        Ok g -> Ok g
        Err (ParsingFailure e) -> Err "ParsingFailure: \(e)"
        Err (ParsingIncomplete e) -> Err "ParsingIncomplete: \(e)"

withIndex : List a -> List (a, Nat)
withIndex = \lst ->
    List.mapWithIndex lst (\a, idx -> (a, idx))

cartProdUnique : List a -> List (a, a)
cartProdUnique = \lst ->
    lstwithIdx = withIndex lst

    List.joinMap
        lstwithIdx
        (\(x, idxX) ->
            List.keepOks
                lstwithIdx
                (\(y, idxY) ->
                    if idxX != idxY && idxX < idxY then Ok (x, y) else Err BadPair
                )
        )

expect cartProdUnique [1, 2, 3] == [(1, 2), (1, 3), (2, 3)]
