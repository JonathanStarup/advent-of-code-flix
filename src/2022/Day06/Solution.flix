mod Day06.Solution {
use Functor.map
use Util.solveFile

pub def main(): Result[IOError, Unit] \ IO = {
    let cwd = "src" / "2022" / "Day06";
    for (
        _ <- solveFile(cwd / "input.txt", cwd / "output1.txt", solveWithCount(4));
        res <- solveFile(cwd / "input.txt", cwd / "output2.txt", solveWithCount(14))
    ) yield res
}

def findMatch(size: Int32, f: Chain[a] -> Bool, l: List[a]): Int32 = region r {
    let index = ref 0 @ r;
    def aux(list, acc) = match list {
        case Nil => ???
        case x :: xs =>
            if (Chain.length(acc) == size) {
                let acc1 = acc |> (tmp -> Chain.snoc(tmp, x)) |> Chain.dropLeft(1);
                if (f(acc1)) {
                    deref index + 1
                } else {
                    index := deref index + 1;
                    aux(xs, acc1)
                }
            } else {
                index := deref index + 1;
                aux(xs, Chain.snoc(acc, x))
            }
    };
    aux(l, Chain.empty())
}

def solveWithCount(count: Int32, lines: List[String]): Int32 = match lines {
    case line :: Nil =>
        findMatch(count, Chain.toSet >> Set.size >> Eq.eq(count), String.toList(line))
    case _ => ???
}

}
