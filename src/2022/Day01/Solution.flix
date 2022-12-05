namespace Day01/Solution {
use Result.flatMap
use Foldable.foldLeft
use Functor.map
use Util.solveFile

pub def main(): Result[Unit, String] \ IO = {
    let cwd = "src" / "2022" / "Day01";
    let* _ = solveFile(cwd / "input.txt", cwd / "output1.txt", solve1);
    solveFile(cwd / "input.txt", cwd / "output2.txt", solve2)
}

def solve1(lines: List[String]): Int32 = region r {
    // None now designates the separation between elves
    // and there is a trailing None
    let parsedLines = lines
        |> map(Int32.fromString)
        |> List.append(None :: Nil);
    let best = ref 0 @ r;
    discard (0, parsedLines) ||> foldLeft((sum, next) -> match next {
        case None =>
            best := Int32.max(deref best, sum);
            0
        case Some(calories) =>
            sum + calories
    });
    deref best
}

def solve2(lines: List[String]): Int32 = region r {
    // None now designates the separation between elves
    // and there is a trailing None
    let parsedLines = lines
        |> map(Int32.fromString)
        |> List.append(None :: Nil);
    let best = ref List.repeat(3, 0) @ r;
    discard (0, parsedLines) ||> foldLeft((sum, next) -> match next {
        case None =>
            best := List.merge(deref best, sum :: Nil) |> List.drop(1);
            0
        case Some(calories) =>
            sum + calories
    });
    deref best |> List.sum
}
}
