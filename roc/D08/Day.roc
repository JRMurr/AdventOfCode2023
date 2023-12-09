interface D08.Day
    exposes [solution]
    imports [
        AoC,
        Util,
        Parser.Core.{ flatten, Parser, oneOf, const, skip, chompWhile, sepBy, oneOrMore, between, map, keep },
        Parser.String.{ Utf8, string, parseStr, scalar, codeunitSatisfies },
    ]

solution : AoC.Solution
solution = { day: 8, part1, part2 }

Instruction : [Left, Right]

NodeId : Str

Node : (NodeId, NodeId)

Network : {
    instructions : List Instruction,
    nodes : Dict NodeId Node,
}

# parseInstructions : Parser _  (List Instruction)
# parseInstructions =

parseNetwork : Str -> Network
parseNetwork = \s ->
    isAlphaNum : U8 -> Bool
    isAlphaNum = \b -> (b >= '0' && b <= '9') || (b >= 'A' && b <= 'Z')

    isSpaceOrNewline = \b -> b == ' ' || b == '\n'

    consumeEndingWhitespace = \parser -> parser |> skip (chompWhile isSpaceOrNewline)

    left = const Left |> skip (string "L")
    right = const Right |> skip (string "R")

    instructions = oneOf [left, right] |> oneOrMore |> consumeEndingWhitespace

    # |> skip (chompUntil '\n')
    # |> skip (codeunit '\n')

    alphaNums =
        oneOrMore (codeunitSatisfies isAlphaNum)
        |> map (\x -> Str.fromUtf8 x |> Result.mapErr (\_ -> "sad alphaNum"))
        |> flatten

    nodeId = consumeEndingWhitespace alphaNums

    betweenParen = \parser -> parser |> between (scalar '(') (scalar ')')

    nodePair =
        betweenParen (sepBy nodeId (string ", "))
        |> map Util.tuplify

    nodeLine =
        const (\id -> \pair -> (id, pair))
        |> keep nodeId
        |> skip (string "= ")
        |> keep nodePair

    nodes =
        sepBy nodeLine (string "\n")
        |> map Dict.fromList

    network =
        const (\i -> \n -> { instructions: i, nodes: n })
        |> keep instructions
        |> keep nodes

    when parseStr network s is
        Ok n -> n
        Err (ParsingFailure e) -> crash "ParsingFailure: \(e)"
        Err (ParsingIncomplete e) -> crash "ParsingIncomplete: \(e)"

walkNetwork : Network, NodeId, U64 -> U64
walkNetwork = \network, currNode, stepNum ->
    if currNode == "ZZZ" then
        stepNum
    else
        { instructions, nodes } = network
        numInstructions = List.len instructions
        instrIdx = ((stepNum |> Num.toNat) % numInstructions)
        currInstruction =
            List.get instructions instrIdx
            |> Util.unwrap

        currPair = Dict.get nodes currNode |> Util.unwrap

        nextNode = if currInstruction == Left then currPair.0 else currPair.1

        walkNetwork network nextNode (stepNum + 1)

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->

    parseNetwork in
    |> walkNetwork "AAA" 0
    |> Num.toStr
    |> Ok

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented
