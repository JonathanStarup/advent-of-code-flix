mod Y2023.Day02.Solution {
    use ToString.toString
    use Functor.map
    use Eq.eq

    pub def main(): Unit \ IO = {
        Util.solveDay(2023, 2, _ -> 42);
        Util.solveDay(2023, 2, _ -> 42)
    }

    type alias CubeBunch = {red = Int32, green = Int32, blue = Int32}

    type alias Game = {id = Int32, pulls = List[CubeBunch]}

    def splitPrefix(r: Regex, s: String): Result[String, (String, String)] = {
        s |>
            Regex.breakAfterFirst(substr = r) |>
            (match (fst, snd) -> if (snd == "") Err("${r} is not in ${s}") else Ok((fst, snd)))
    }

    def _parseGame(s: String): Result[String, Game] = {
        for (
            gameSuffix <- s |>
                Regex.stripPrefix(substr = regex"Game\\s*") |>
                Util.toOk(_ -> "no game prefix in ${s}");
            (id, idSuffix) <- gameSuffix |> parseNumber;
            colonSuffix <- idSuffix |>
                Regex.stripPrefix(substr = regex":\\s*") |>
                Util.toOk(_ -> "couldnt colon from ${idSuffix}");
            pullsReverse <- colonSuffix |> parsePulls
        ) yield {id = id, pulls = pullsReverse |> List.reverse}
    }

    def parseNumber(s: String): Result[String, (Int32, String)] = {
        for (
            (numberPart, numberSuffix) <- s |> splitPrefix(regex"\\d+");
            number <- Int32.fromString(numberPart) |>
                Util.toOk(_ -> "${numberPart} is not a number")
        ) yield (number, numberSuffix)
    }

    def parsePulls(s: String): Result[String, List[CubeBunch]] = {
        parsePull(s) |> Result.flatMap(optPull -> {
            match optPull {
                case None => Ok(Nil)
                case Some((pull, tail)) =>
                    for (
                        pulls <- parsePulls(tail)
                    ) yield pull :: pulls
            }
        })
    }

    def parsePull(s: String): Result[String, Option[(CubeBunch, String)]] = {
        let cb1 = {red = 0, green = 0, blue = 0};
        for (
            (sb2, color) <- s |> parseColorPull(cb1)
        ) yield ???
    }

    def parseColorPull(cb: CubeBunch, s: String): Result[String, (CubeBunch, String)] = {
        for (
            commaSuffix <- s |> Regex.stripPrefix(substr = regex"(,\\s*)?") |>
                Util.toOk(_ -> "could not strip prefix");
            (number, numberSuffix) <- commaSuffix |> parseNumber;
            (colorString, colorSuffix) <- numberSuffix |> splitPrefix(regex"\\w+")
        ) yield ???
    }





}
