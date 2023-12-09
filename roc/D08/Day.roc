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

NodeId : (U8, U8, U8)

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
        |> map (Util.tuplify3)
    # |> map (\x -> Str.fromUtf8 x |> Result.mapErr (\_ -> "sad alphaNum"))
    # |> flatten

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
    if currNode == ('Z', 'Z', 'Z') then
        stepNum
    else
        currInstruction = getCurrInstr network stepNum
        nextNode = getNextNode (network.nodes) currNode currInstruction

        walkNetwork network nextNode (stepNum + 1)

getCurrInstr : Network, U64 -> Instruction
getCurrInstr = \network, stepNum ->
    { instructions } = network
    numInstructions = List.len instructions
    instrIdx = ((stepNum |> Num.toNat) % numInstructions)
    List.get instructions instrIdx
    |> Util.unwrap

getNextNode = \nodes, currNode, currInstruction ->
    currPair = Dict.get nodes currNode |> Util.unwrap
    if currInstruction == Left then currPair.0 else currPair.1

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \in ->
    parseNetwork in
    |> walkNetwork ('A', 'A', 'A') 0
    |> Num.toStr
    |> Ok

# walkP2 : Network, List NodeId, U64 -> U64
# walkP2 = \network, currNodes, stepNum ->
#     if List.all currNodes (\(_, _, x) -> x == 'Z') then
#         stepNum
#     else
#         currInstruction = getCurrInstr network stepNum
#         nextNodes = List.map
#             currNodes
#             (\n ->
#                 getNextNode (network.nodes) n currInstruction
#             )
#         walkP2 network nextNodes (stepNum + 1)

walkP2 : Network, NodeId, U64 -> U64
walkP2 = \network, currNode, stepNum ->
    if currNode.2 == 'Z' then
        stepNum
    else
        currInstruction = getCurrInstr network stepNum
        nextNode = getNextNode (network.nodes) currNode currInstruction
        walkP2 network nextNode (stepNum + 1)

leastCommonMultipleInList = \list ->
    List.walk list 1 \lcm, num ->
        leastCommonMultiple lcm num

leastCommonMultiple = \a, b ->
    (a * b) // (greatestCommonDivisor a b)

greatestCommonDivisor = \a, b ->
    if b == 0 then
        a
    else
        greatestCommonDivisor b (a % b)


part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \in ->
    network = parseNetwork in

    startNodes =
        Dict.keys (network.nodes)
        |> List.keepIf (\(_, _, x) -> x == 'A')

    startNodes
    |> List.map (\n -> walkP2 network n 0)
    |> leastCommonMultipleInList
    |> Num.toStr
    |> Ok
