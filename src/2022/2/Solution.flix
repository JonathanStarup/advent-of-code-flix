namespace Two {
    use Functor.map
    use Util.solveFile
    use Result.flatMap

    pub def main(): Result[Unit, String] \ IO = {
        let cwd = "src" / "2022" / "2";
        let* _ = solveFile(cwd / "input.txt", cwd / "output1.txt", solve1);
        solveFile(cwd / "input.txt", cwd / "output2.txt", solve2)
    }

    enum Rps with Eq, ToString{
        case Rock, Paper, Scissor
    }

    def stringToRps(s: String): Rps = match s {
        case "A" => Rock
        case "B" => Paper
        case "C" => Scissor
        case "X" => Rock
        case "Y" => Paper
        case "Z" => Scissor
        case _   => bug!("'${s}' is not an Rps")
    }

    enum Outcome with Eq, ToString {
        case Win, Tie, Loss
    }

    def beats(p1: Rps, p2: Rps): Outcome = match (p1, p2) {
        case (Paper, Rock)    => Win
        case (Rock, Scissor)  => Win
        case (Scissor, Paper) => Win
        case _ if p1 == p2    => Tie
        case _                => Loss
    }

    def beatPoints(p1: Rps, p2: Rps): Int32 = match beats(p1, p2) {
        case Win  => 6
        case Tie  => 3
        case Loss => 0
    }

    def shapePoints(p: Rps): Int32 = match p {
        case Rock    => 1
        case Paper   => 2
        case Scissor => 3
    }

    def points(p1: Rps, p2: Rps): Int32 = {
        beatPoints(p1, p2) + shapePoints(p1)
    }

    def parseRpsPair(s: String): (Rps, Rps) = {
        let l = s |> String.split(regex = " ") |>
            map(stringToRps);
        match l {
            case one :: two :: Nil => (one, two)
            case _ => bug!("${l} does not have two elements")
        }
    }

    def solve1(lines: List[String]): Int32 = {
        lines |> map(parseRpsPair) |> map(
            match (p1, p2) -> points(p2, p1)
        ) |> List.sum
    }

    def stringToOutcome(s: String): Outcome = match s {
        case "X" => Loss
        case "Y" => Tie
        case "Z" => Win
        case _   => bug!("'${s}' is not an outcome")
    }

    def parseRpsOutcomePair(s: String): (Rps, Outcome) = {
        let l = s |> String.split(regex = " ");
        match l {
            case one :: two :: Nil =>
                (stringToRps(one), stringToOutcome(two))
            case _ => bug!("${l} does not have two elements")
        }
    }

    def achieveAgainst(out: Outcome, p: Rps): Rps = match (out, p) {
        case (Tie, _)        => p
        case (Win, Rock)     => Paper
        case (Win, Paper)    => Scissor
        case (Win, Scissor)  => Rock
        case (Loss, Rock)    => Scissor
        case (Loss, Paper)   => Rock
        case (Loss, Scissor) => Paper
    }

    def solve2(lines: List[String]): Int32 = {
        lines |> map(parseRpsOutcomePair) |> map(
            match (p, out) -> {
                let myP = achieveAgainst(out, p);
                points(myP, p)
            }
        ) |> List.sum
    }

}
