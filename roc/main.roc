app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Arg,
        pf.Task.{ Task },
        pf.Utc.{ Utc },
        pf.Path,
        pf.File,
        ANSI,
        App,
        AoC,
        # Util,
    ]
    provides [main] to pf

main : Task {} *
main = runTask |> Task.onErr handlErr

handlErr : [UnableToParseArgs Str, SolutionNotFound, NotFound] -> Task {} *
handlErr = \err ->
    when err is
        UnableToParseArgs msg -> Stdout.line msg
        NotFound -> Stdout.line "input not found"
        SolutionNotFound -> Stdout.line "solution not found"

partToStr = \p ->
    when p is
        Part1 -> "Part 1"
        Part2 -> "Part 2"

runTask : Task {} [UnableToParseArgs Str, SolutionNotFound, NotFound]
runTask =

    { dayArg, partArg, inputType } <- getArgs |> Task.await

    input <- readDaysInput { day: dayArg, inputType } |> Task.await

    solution <-
        (App.findSolution dayArg)
        |> Task.fromResult
        |> Task.await

    runPart : [Part1, Part2] -> Task { res : AoC.PartRes, time : U128 } *
    runPart = \puzzle ->
        {} <- Stdout.write (ANSI.withFg "Running \(partToStr puzzle)..." Gray) |> Task.await
        start <- Utc.now |> Task.await
        res = App.solvePuzzle { solution, input, puzzle }
        end <- Utc.now |> Task.await
        {} <- Stdout.write (ANSI.withFg "done\n" Gray) |> Task.await
        time = Utc.deltaAsMillis start end
        Task.ok { res, time }

    skipPart =
        Task.ok { res: Ok "SKIPPED PART", time: 0 }

    handleRunPart : [Part1, Part2] -> Task { res : AoC.PartRes, time : U128 } *
    handleRunPart = \puzzle ->
        when (partArg, puzzle) is
            (Both, _) -> runPart puzzle
            (Part1, Part1) -> runPart puzzle
            (Part2, Part2) -> runPart puzzle
            _ -> skipPart

    { res: partOneResult, time: p1Time } <- handleRunPart Part1 |> Task.await

    { res: partTwoResult, time: p2Time } <- handleRunPart Part2 |> Task.await

    day = ANSI.withFg "\(Num.toStr dayArg)" Blue
    part1 = solutionResultToStr partOneResult
    part2 = solutionResultToStr partTwoResult
    part1Time = ANSI.withFg (deltaToStr p1Time) Blue
    part2Time = ANSI.withFg (deltaToStr p2Time) Blue
    totalTime = ANSI.withFg (deltaToStr (p1Time + p2Time)) Blue

    """
    ---------------------------------
    day: \(day)
    total time: \(totalTime)

    Part 1 calculated in \(part1Time)
    ---------------------------------
    \(part1)

    Part 2 calculated in \(part2Time)
    ---------------------------------
    \(part2)

    """
    |> Stdout.line

listToStr : List Str -> Str
listToStr = \lst ->
    joined = Str.joinWith lst ", "
    "[\(joined)]"

toU8Unsafe : Str -> U8
toU8Unsafe = \s ->
    when Str.toU8 s is
        Ok n -> n
        Err _ -> crash "invalid u8: \(s)"

PartSelection : [
    Part1,
    Part2,
    Both,
]

partSelectionParse : Str -> PartSelection
partSelectionParse = \s ->
    when s is
        "1" -> Part1
        "2" -> Part2
        _ -> Both

InputType : [Normal, Example, StdIn]

inputTypeParse : Str -> InputType
inputTypeParse = \s ->
    when s is
        "1" -> Example
        "true" -> Example
        "-" -> StdIn
        _ -> Normal

getArgs : Task
        {
            dayArg : U8,
            partArg : PartSelection,
            inputType : InputType,
        }
        [UnableToParseArgs Str]_
getArgs =
    args <- Task.await Arg.list
    # drop first arg since its the binary/program name
    droppedArgs = List.dropFirst args 1
    when droppedArgs is
        [day, part, inputType] -> Task.ok { dayArg: toU8Unsafe day, partArg: partSelectionParse part, inputType: inputTypeParse inputType }
        _ ->
            dbg
                args

            # Task.ok { dayArg: 10, partArg: partSelectionParse "1", inputType: inputTypeParse "0" }
            Task.err (UnableToParseArgs "bad arguments: \(listToStr droppedArgs)")

solutionResultToStr : Result Str [NotImplemented, Error Str] -> Str
solutionResultToStr = \result ->
    when result is
        Ok answer -> answer
        Err NotImplemented -> "not yet implemented"
        Err (Error msg) -> "returned an error: \(msg)"

deltaToStr : U128 -> Str
deltaToStr = \millis ->
    # millis = Utc.deltaAsMillis start end
    if millis == 0 then
        "<0 ms"
    else
        Num.toStr millis

readDayFile : U8, Str -> Task Str [NotFound]
readDayFile = \day, fname ->
    dayStr = Num.toStr day
    dayFormatted = if day < 10 then "0\(dayStr)" else dayStr

    path = Path.fromStr "./D\(dayFormatted)/\(fname)"
    File.readUtf8 path
    |> Task.onErr \_ -> Task.err NotFound

readDaysInput : { day : U8, inputType : InputType } -> Task Str [NotFound]
readDaysInput = \x ->
    when x.inputType is
        StdIn -> crash "todo: std in read"
        Normal -> readDayFile x.day "in"
        Example -> readDayFile x.day "in.example"
