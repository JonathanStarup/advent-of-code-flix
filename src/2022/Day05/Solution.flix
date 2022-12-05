namespace Day05/Solution {
use Functor.map
use Util.solveFile

pub def main(): Result[Unit, String] \ IO = region r {
    use Result.flatMap;
    let cwd = "src" / "2022" / "Day05";
    let* _ = solveFile(cwd / "input.txt", cwd / "output1.txt", solveWithMove(r, move9000));
    solveFile(cwd / "input.txt", cwd / "output2.txt", solveWithMove(r, move9001))
}

type alias Move = {
    src    = Int32,
    target = Int32,
    amount = Int32
}

type alias Crate = Char

type alias Stack[r: Region] = MutList[Crate, r]

type alias Yard[r: Region] = Array[Stack[r], r]

def crash(opt: Option[t]): t = match opt {
    case Some(v) => v
    case None => ???
}

def move9000(move: Move, yard: Yard[r]): Unit \ r = region local {
    use Array.nth;
    let target = nth(move.target, yard) |> crash;
    let src = nth(move.src, yard) |> crash;
    foreach(_ <- Iterator.range(local, 0, move.amount)) {
        let crate = MutList.pop!(src) |> crash;
        MutList.push!(crate, target)
    }
}

def stringToMove(s: String): Move = region r {
    use Monad.flatMap;
    let words = String.words(s) |> List.toArray(r);
    def extract(i) =
        words |>
        Array.nth(i) |>
        flatMap(Int32.fromString) |>
        crash;
    {src = extract(3)-1, target = extract(5)-1, amount = extract(1)}
}

def stringToCrate(s: String): Option[Crate] = region r {
    if (s == "   ") None
    else if (String.isMatch(regex = "\\[[A-Z]\\]", s)) {
        String.toList(s) |> List.toArray(r) |>
            Array.nth(1) |> crash |> Some
    } else None
}

def linesToYard(r: Region[r], stacks: Int32, lines: List[String]): Yard[r] \ r = region local {
    let yard = Array.init(r, _ -> MutList.new(r), stacks);
    foreach(line <- List.iterator(local, lines)) {
        def inner(s, i) = match s {
            case "" => ()
            case _ =>
                let crate = String.take(3, s) |> stringToCrate;
                crate |> Option.forEach(c ->
                    MutList.push!(c, Array.nth(i, yard) |> crash)
                );
                inner(String.dropLeft(4, s), i+1)
        };
        inner(line, 0)
    };
    Array.forEach(MutList.reverse!, yard);
    yard
}

def solveWithMove(r: Region[r], move: (Move, Yard[r]) -> Unit \ ef, lines: List[String]): String \ {r, ef} = {
    let (aLines, bLines) = lines |> List.splitAt(List.indexOf("", lines) |> crash);
    let yardLines = aLines |> List.reverse |> List.drop(1) |> List.reverse;
    let moveLines = bLines |> List.drop(1);
    let yard = linesToYard(r, 9, yardLines);
    let moves = moveLines |> map(stringToMove);
    moves |> List.forEach(m -> move(m, yard));
    let topCrates = yard |> Array.map(stack -> MutList.pop!(stack) |> crash);
    Array.join("", topCrates)
}

def move9001(move: Move, yard: Yard[r]): Unit \ r = region local {
    use Array.nth;
    let src = nth(move.src, yard) |> crash;
    let target = nth(move.target, yard) |> crash;
    let tmp = MutList.new(local);
    foreach(_ <- Iterator.range(local, 0, move.amount)) {
        let crate = MutList.pop!(src) |> crash;
        MutList.push!(crate, tmp)
    };
    foreach(_ <- Iterator.range(local, 0, move.amount)) {
        let crate = MutList.pop!(tmp) |> crash;
        MutList.push!(crate, target)
    }
}

}
