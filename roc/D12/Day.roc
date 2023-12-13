interface D12.Day
    exposes [solution]
    imports [
        AoC,
        Util,
        Parser.Core.{ Parser, oneOf, oneOrMore, const, skip, keep, sepBy },
        Parser.String.{ parseStr, scalar, digits },
    ]

solution : AoC.Solution
solution = { day: 12, part1, part2 }

Spring : [Working, Broken, Unknown]

Row : {
    springs : List Spring,
    groups : List Nat,
}

parseRows : Str -> Result (List Row) Str
parseRows = \s ->
    spring = oneOf [
        const Working |> skip (scalar '.'),
        const Broken |> skip (scalar '#'),
        const Unknown |> skip (scalar '?'),
    ]

    damagedSizesP = digits |> sepBy (scalar ',')

    row =
        const (\springs -> \damagedSizes -> { springs, groups: damagedSizes })
        |> keep (oneOrMore spring)
        |> skip (scalar ' ')
        |> keep damagedSizesP

    rows = row |> sepBy (scalar '\n')

    when parseStr rows (Str.trim s) is
        Ok res -> Ok res
        Err (ParsingFailure e) -> Err "ParsingFailure: \(e)"
        Err (ParsingIncomplete e) -> Err "ParsingIncomplete: \(e)"

# SimpleRow : {
#     chunks : List (List [Broken, Unknown]),
#     damagedSizes : List Nat,
# }

# # split on working to get chunks of broken/unknown
# # find all chunks of just broken to reduce the damagedSizes (if possible)
# simplifyRow : Row -> SimpleRow
# simplifyRow = \row ->
#     walkRes = List.walk
#         row.springs
#         ({ curr: [], acc: [] })
#         (\{ curr, acc }, spring ->
#             when (spring, curr) is
#                 (Working, []) -> { curr, acc }
#                 (Working, lst) -> { curr: [], acc: List.append acc lst }
#                 # explict check on spring to filter the tag union here
#                 (Broken, lst) -> { curr: List.append lst Broken, acc }
#                 (Unknown, lst) -> { curr: List.append lst Unknown, acc }
#         )
#     chunks =
#         when walkRes.curr is
#             [] -> walkRes.acc
#             lst -> List.append walkRes.acc lst

#     { chunks, damagedSizes: [] }

# ???...?..????. 1,1

Cache : Dict Row Nat

countPossibleHelper : Row, Cache -> (Cache, Nat)
countPossibleHelper = \{ springs, groups }, cache ->
    # insertAndRet = \d, v ->
    #     (Dict.insert d { springs, groups } v, v)

    # returnNoPossible = \{} -> insertAndRet cache 0

    containsWorking : List Spring -> Bool
    containsWorking = \lst -> List.contains lst Working

    idxIsBroken : List Spring, Nat -> Bool
    idxIsBroken = \lst, idx ->
        when List.get lst idx is
            Ok Broken -> Bool.true
            _ -> Bool.false

    go = \{} ->
        when List.first groups is
            Err _ ->
                if
                    List.any springs (\p -> p == Broken)
                then
                    # returnNoPossible {}
                    (Dict.insert cache { springs, groups } 0, 0)
                else
                    # insertAndRet cache 1
                    (Dict.insert cache { springs, groups } 1, 1)

            Ok group -> handleSingleGroup group

    handleSingleGroup = \group ->
        firstPossibleBrokenRes = List.findFirstIndex springs (\p -> p == Broken || p == Unknown)
        when firstPossibleBrokenRes is
            Err _ -> (Dict.insert cache { springs, groups } 0, 0)
            Ok n ->
                springGroup = List.sublist springs { start: n, len: group }
                if
                    List.len springGroup != group
                then
                    (Dict.insert cache { springs, groups } 0, 0)
                else
                    invalidPerm = containsWorking springGroup || idxIsBroken springs (n + group)
                    if
                        invalidPerm
                    then
                        (
                            if
                                List.get springs n == Ok Broken
                            then
                                (Dict.insert cache { springs, groups } 0, 0)
                            else
                                x = countPossibleHelper { springs: List.dropFirst springs (n + 1), groups } cache
                                # insertAndRet x.0 x.1
                                (Dict.insert x.0 { springs, groups } x.1, x.1)
                        )
                        # current perm is the same size as the group we want and it only has unknowns or brokens
                    else
                        res1 = (
                            if
                                List.get springs n == Ok Broken
                            then
                                (Dict.insert cache { springs, groups } 0, 0)
                                # no extra since the current idx we are looking at is fixed
                            else
                                # curr is unknown so lets treat it as empty and see if this group size can still appear
                                x = countPossibleHelper { springs: List.dropFirst springs (n + 1), groups } cache
                                # insertAndRet x.0 x.1
                                (Dict.insert x.0 { springs, groups } x.1, x.1)
                        )

                        droppedSprings = List.dropFirst springs (n + group + 1)
                        droppedGroups = List.dropFirst groups 1
                        res2 = countPossibleHelper { springs: droppedSprings, groups: droppedGroups } res1.0

                        # insertAndRet res2.0 (res2.1 + res1.1)
                        (Dict.insert res2.0 { springs, groups } (res2.1 + res1.1), (res2.1 + res1.1))

    when Dict.get cache { springs, groups } is
        Ok res -> (cache, res)
        Err _ -> go {}

countPossible : Row -> Nat
countPossible = \row ->
    countPossibleHelper row (Dict.empty {}) |> .1

# expect countPossible { springs: [Unknown, Unknown, Unknown, Working, Broken, Broken, Broken], groups: [1, 1, 3] } == 1

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    in
    |> parseRows
    |> Util.unwrap
    |> List.map countPossible
    |> List.sum
    |> Num.toStr
    |> Ok

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \in ->
    in
    |> parseRows
    |> Util.unwrap
    |> List.map (\{springs, groups} ->
        repeatAndJoin = \lst, fill -> 
            lst |> List.repeat 5 |> List.intersperse fill |> List.join
        newSprings = repeatAndJoin springs [Unknown]

        newGroups = groups |> List.repeat 5 |> List.join
        
        {springs: newSprings, groups: newGroups}
    )
    |> List.map countPossible
    |> List.sum
    |> Num.toStr
    |> Ok
