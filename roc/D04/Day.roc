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

parseCardId : Str -> Result U64 Str
parseCardId = \prefix ->
    when Str.split prefix " " is
        ["Card", id] -> Result.mapErr (Str.toU64 id) (\_ -> "bad card id num")
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

        _ -> Err "bad first split "

expect
    parseLine "Card 1: 41 48 83 | 83 86  6 31"
    ==
    Ok { id: 1, mine: Set.fromList [83, 86, 6, 31], winners: Set.fromList [41, 48, 83] }

calcPoints : Card -> U64
calcPoints = \c -> crash "TODO:"

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    # Str.split in "\n"
    #     |> List.map parseLine

    Ok
        "123"

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
