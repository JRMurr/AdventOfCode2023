interface D04.Day
    exposes [solution]
    imports [AoC]

solution : AoC.Solution
solution = { day: 4, part1, part2 }

Card : {
    id : U64,
    mine : Set U64,
    winners : Set U64,
}

parseSpaceSepNums : Str -> Set U64
parseSpaceSepNums = \s ->
    Str.split s " "
    |> List.keepOks Str.toU64
    |> Set.fromList

expect parseSpaceSepNums "10 20 45" == Set.fromList [10u64, 45, 20]

expect
    parseSpaceSepNums "90 30 42 98  3 59 64 92 93 58 52 86 23 49 37  5 34 31 95  6  7  4 74 43  2"
    == Set.fromList [90, 30, 42, 98, 3, 59, 64, 92, 93, 58, 52, 86, 23, 49, 37, 5, 34, 31, 95, 6, 7, 4, 74, 43, 2]

parseCardId : Str -> Result U64 Str
parseCardId = \prefix ->
    when Str.split prefix " " is
        ["Card", .., id] -> Result.mapErr (Str.toU64 id) (\_ -> "bad card id num")
        _ -> Err "bad card str"

expect parseCardId "Card 4" == Ok 4

parseFirstHalf : Str -> Result (U64, Set U64) Str
parseFirstHalf = \s ->
    when Str.split s ": " is
        [cardStr, nums] ->
            (
                parseCardId cardStr
                |> Result.map
                    (\id ->
                        setNums = parseSpaceSepNums nums
                        (id, setNums)
                    )
            )

        _ -> Err "invalid first half"

parseLine : Str -> Result Card Str
parseLine = \s ->
    when Str.split s " | " is
        [first, second] ->
            (
                parseFirstHalf first
                |> Result.map
                    (\parsedFirst ->
                        myNums = parseSpaceSepNums second
                        { id: parsedFirst.0, winners: parsedFirst.1, mine: myNums }
                    )
            )

        _ ->
            dbg
                s

            crash "sad \(s)"

expect
    parseLine "Card 1: 41 48 83 | 83 86  6 31"
    ==
    Ok { id: 1, mine: Set.fromList [83, 86, 6, 31], winners: Set.fromList [41, 48, 83] }

calcPoints : Card -> U64
calcPoints = \c ->
    numMatches =
        Set.intersection c.mine c.winners
        |> Set.len
        |> Num.toU64
    when numMatches is
        0 -> 0
        x -> Num.powInt 2 (x - 1)

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    parseDebug = \x ->
        parseLine x
        |> Result.mapErr
            (\e ->
                dbg
                    { x, e }

                e
            )

    Str.split in "\n"
    |> List.keepOks parseDebug
    |> List.map calcPoints
    |> List.sum
    |> Num.toStr
    |> Ok

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \in ->

    reducer : Dict U64 U64, Card -> Dict U64 U64
    reducer = \acc, c ->
        numCards = calcPoints c

        range : List U64
        range = List.range { start: After c.id, end: Length numCards, step: 1 }

        copies =
            range
            |> List.map
                (\id ->
                    acc.get id
                    |> Result.withDefault 0
                )

        Dict.insert c.id copies

    Str.split in "\n"
    |> List.keepOks parseLine
    |> List.walkBackwards
        (Dict.empty {})
        reducer
    |> Dict.get 1
    |> Result.withDefault -1
    |> Num.toStr
    |> Ok
