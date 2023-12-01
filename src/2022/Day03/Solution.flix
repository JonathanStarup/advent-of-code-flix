mod Day03.Solution {
use Functor.map
use Util.solveFile
use Result.flatMap
use Set.intersection
use Set.minimum

pub def main(): Result[IOError, Unit] \ IO = {
    let cwd = "src" / "2022" / "Day03";
    for (
        _ <- solveFile(cwd / "input.txt", cwd / "output1.txt", solve1);
        res <- solveFile(cwd / "input.txt", cwd / "output2.txt", solve2)
    ) yield res
}

def letterPoints(c: Char): Int32 = {
    if (Char.isLowerCase(c)) {
        ?charToInt32(c) + 1 - ?charToInt32('a')
    } else {
        ?charToInt32(c) + 27 - ?charToInt32('A')
    }
}

def stringToSet(s: String): Set[Char] =
    s |> String.toList |> List.toSet

def solve1(lines: List[String]): Int32 = {
    use String.{length, splitAt};

    lines |>
        map(line ->
            let middle = length(line) / 2;
            let (sack1, sack2) = splitAt(middle, line);
            let diffOpt = minimum(
                stringToSet(sack1) `intersection` stringToSet(sack2)
            );
            match diffOpt {
                case None    => ???
                case Some(c) => letterPoints(c)
             }
        ) |> List.sum
}

def solve2(lines: List[String]): Int32 = {
    solve2Aux(map(stringToSet, lines), 0)
}

def solve2Aux(bags: List[Set[Char]], sum: Int32): Int32 = match bags {
    case s1 :: s2 :: s3 :: rest =>
        let diffOpt = minimum(s1 `intersection` s2 `intersection` s3);
        let points = match diffOpt {
            case None    => ???
            case Some(c) => letterPoints(c)
        };
        solve2Aux(rest, points + sum)
    case Nil => sum
    case _ => bug!("${bags}")
}

}
