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

parseHand : Str -> Hand
parseHand = \s ->
    (cardStr, bid) =
        Str.split s " "
        |> Util.tuplify

    { cards: Str.graphemes cardStr, bid: Util.toU64Unsafe bid }

expect parseHand "32T3K 765" == { cards: ["3", "2", "T", "3", "K"], bid: 765 }

classifyHand : Hand

# compareHands : Hand,Hand -> [LT,EQ,GT]
# compareHands = \x,y ->

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \_ -> Err NotImplemented

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
