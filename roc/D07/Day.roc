interface D07.Day
    exposes [solution]
    imports [AoC, Util]

solution : AoC.Solution
solution = { day: 7, part1, part2 }

Card : Str

Hand : {
    cards : List Card,
    bid : U64,
}

# order from best to worst
HandType : [FiveKind, FourKind, FullHouse, ThreeKind, TwoPair, OnePair, HighCard]

parseHand : Str -> Hand
parseHand = \s ->
    (cardStr, bid) =
        Str.split s " "
        |> Util.tuplify

    { cards: Str.graphemes cardStr, bid: Util.toU64Unsafe bid }

expect parseHand "32T3K 765" == { cards: ["3", "2", "T", "3", "K"], bid: 765 }

classifyHand : Hand -> HandType
classifyHand = \h ->
    counts = Util.countElems h.cards

    getMinMaxValues : Dict * (Num a) -> (Num a, Num a)
    getMinMaxValues = \d ->
        vals = Dict.values d

        (vals |> List.min |> Util.unwrap, vals |> List.max |> Util.unwrap)

    when getMinMaxValues counts is
        (_, 5) -> FiveKind
        (1, 4) -> FourKind
        (2, 3) -> FullHouse
        (1, 3) -> ThreeKind
        (1, 2) if Dict.len counts == 3 -> TwoPair
        (1, 2) if Dict.len counts == 4 -> OnePair
        (1, 1) -> HighCard
        _ ->
            dbg
                h

            crash "bad counts"

parseAndClassify = \s ->
    parseHand s |> classifyHand

expect parseAndClassify "AAAAA 1" == FiveKind
expect parseAndClassify "AA8AA 1" == FourKind
expect parseAndClassify "23332 1" == FullHouse
expect parseAndClassify "TTT98 1" == ThreeKind
expect parseAndClassify "23432 1" == TwoPair
expect parseAndClassify "A23A4 1" == OnePair
expect parseAndClassify "23456 1" == HighCard

compareTypes : HandType, HandType -> [LT, EQ, GT]
compareTypes = \x, y ->
    toNum = \h ->
        when h is
            FiveKind -> 7
            FourKind -> 6
            FullHouse -> 5
            ThreeKind -> 4
            TwoPair -> 3
            OnePair -> 2
            HighCard -> 1

    Num.compare (toNum x) (toNum y)

compareCards : Card, Card -> [LT, EQ, GT]
compareCards = \x, y ->
    toNum = \c ->
        when c is
            "2" -> 1
            "3" -> 2
            "4" -> 3
            "5" -> 4
            "6" -> 5
            "7" -> 6
            "8" -> 7
            "9" -> 8
            "T" -> 9
            "J" -> 10
            "Q" -> 11
            "K" -> 12
            "A" -> 13
            _ -> crash "bad card: \(c)"
    Num.compare (toNum x) (toNum y)

compareHands : Hand, Hand -> [LT, EQ, GT]
compareHands = \x, y ->
    typeComp = compareTypes (classifyHand x) (classifyHand y)
    if typeComp != EQ then
        typeComp
    else
        zipped = List.map2 x.cards y.cards (\a, b -> (a, b))
        List.walkUntil
            zipped
            EQ
            (\acc, (a, b) ->
                compRes = compareCards a b
                if compRes != EQ then Break compRes else Continue acc
            )

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    Str.split in "\n"
    |> List.map parseHand
    |> List.sortWith compareHands
    |> List.mapWithIndex
        (\h, idx ->
            h.bid * ((idx + 1) |> Num.toU64)
        )
    |> List.sum
    |> Num.toStr
    |> Ok

maxKey : Dict Str U64 -> Str
maxKey = \d ->
    Dict.toList d
    |> List.sortWith (\p1, p2 -> Num.compare p1.1 p2.1)
    |> List.first
    |> Util.unwrap
    |> .0

countHandleJoker : Hand -> Dict Str U64
countHandleJoker = \h ->
    counts = Util.countElems h.cards
    when Dict.get counts "J" is
        Err _ -> counts
        Ok 5 -> counts # already max
        Ok jCount ->
            droppedJdict = Dict.remove counts "J"

            Dict.update
                droppedJdict
                (maxKey droppedJdict)
                (\v ->
                    when v is
                        Present x -> Present (x + jCount)
                        _ -> Missing
                )

classifyHandP2 : Hand -> HandType
classifyHandP2 = \h ->
    counts = countHandleJoker h

    getMinMaxValues : Dict * (Num a) -> (Num a, Num a)
    getMinMaxValues = \d ->
        vals = Dict.values d

        (vals |> List.min |> Util.unwrap, vals |> List.max |> Util.unwrap)

    when getMinMaxValues counts is
        (_, 5) -> FiveKind
        (1, 4) -> FourKind
        (2, 3) -> FullHouse
        (1, 3) -> ThreeKind
        (1, 2) if Dict.len counts == 3 -> TwoPair
        (1, 2) if Dict.len counts == 4 -> OnePair
        (1, 1) -> HighCard
        _ ->
            dbg
                h

            crash "bad counts"

compareCardsP2 : Card, Card -> [LT, EQ, GT]
compareCardsP2 = \x, y ->
    toNum = \c ->
        when c is
            "J" -> 0
            "2" -> 1
            "3" -> 2
            "4" -> 3
            "5" -> 4
            "6" -> 5
            "7" -> 6
            "8" -> 7
            "9" -> 8
            "T" -> 9
            "Q" -> 11
            "K" -> 12
            "A" -> 13
            _ -> crash "bad card: \(c)"
    Num.compare (toNum x) (toNum y)

compareHandsP2 : Hand, Hand -> [LT, EQ, GT]
compareHandsP2 = \x, y ->
    typeComp = compareTypes (classifyHandP2 x) (classifyHandP2 y)
    if typeComp != EQ then
        typeComp
    else
        zipped = List.map2 x.cards y.cards (\a, b -> (a, b))
        List.walkUntil
            zipped
            EQ
            (\acc, (a, b) ->
                compRes = compareCardsP2 a b
                if compRes != EQ then Break compRes else Continue acc
            )

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \in ->
    Str.split in "\n"
    |> List.map parseHand
    |> List.sortWith compareHandsP2
    |> List.mapWithIndex
        (\h, idx ->
            h.bid * ((idx + 1) |> Num.toU64)
        )
    |> List.sum
    |> Num.toStr
    |> Ok
