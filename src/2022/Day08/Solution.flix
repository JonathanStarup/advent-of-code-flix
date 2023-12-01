mod Day08.Solution {
use Functor.map
use Util.solveFile
use ToString.toString
use FromString.fromString

pub def main(): Result[IOError, Unit] \ IO = {
    use Result.flatMap;
    let cwd = "src" / "2022" / "Day08";
    for (
        _ <- solveFile(cwd / "input.txt", cwd / "output1.txt", solve1 >> resPanic);
        res <- solveFile(cwd / "input.txt", cwd / "output2.txt", solve2)
    ) yield res
}

def resPanic(r: Result[String, t]): t = match r {
    case Ok(v) => v
    case Err(msg) => bug!(msg)
}

// =============================================================================
// Datalog solution
// =============================================================================

def flatMapWithIndex(f: Int32 -> a -> List[b] \ ef, l: List[a]): List[b] \ ef = {
    l |> List.zipWithIndex |> List.flatMap(match (index, x) -> f(index)(x))
}

def parseInput(lines: List[String]): Result[String, List[(Int32, Int32, Int32)]] = {
    use List.mapWithIndex;
    use Option.toOk;
    lines |> flatMapWithIndex(x ->
        String.toList >>
        mapWithIndex(y ->
            toString >>
            Int32.fromString >>
            toOk("NaN") >>
            Result.map(height -> (x, y, height))
        )
    ) |> Result.sequence
}

enum Direction with Eq, Order {
    case North, East, South, West
}

def solve1(lines: List[String]): Result[String, Int32] = {
    use Result.flatMap;
    parseInput(lines) |> Result.map(heights ->
        let rows = List.length(lines);
        let cols = List.head(lines) |> map(String.length) |> Option.getWithDefault(0);
        let heightData = inject heights into Height;
        let lp = #{
            // outside trees are always visible
            Visible(x, y) :- Height(x, y, _), if x == 0.
            Visible(x, y) :- Height(x, y, _), if y == 0.
            Visible(x, y) :- Height(x, y, _), if x == rows-1.
            Visible(x, y) :- Height(x, y, _), if y == cols-1.
            // base sightlines
            SightLine(x, y, Direction.North, h) :- Height(x, y, h), if x == 0.
            SightLine(x, y, Direction.West , h) :- Height(x, y, h), if y == 0.
            SightLine(x, y, Direction.South, h) :- Height(x, y, h), if x == rows-1.
            SightLine(x, y, Direction.East , h) :- Height(x, y, h), if y == cols-1.
            // neighbors; <x, y> is <direction> of <p, q>
            DirOf(x-1, y  , Direction.North, x, y) :- Height(x, y, _), if x > 0.
            DirOf(x+1, y  , Direction.South, x, y) :- Height(x, y, _), if x < rows-1.
            DirOf(x  , y-1, Direction.West , x, y) :- Height(x, y, _), if y > 0.
            DirOf(x  , y+1, Direction.East , x, y) :- Height(x, y, _), if y < cols-1.
            // sightline in (x,y) has hight h from dir (i.e. max height in a direction)
            SightLine(x, y, dir, Int32.max(h, local)) :- DirOf(p, q, dir, x, y),
                                    SightLine(p, q, dir, h), Height(x, y, local).
            // visible if the sightline is the trees hight and the previous
            // sightline was lower
            Visible(x, y) :- DirOf(p, q, dir, x, y), SightLine(x, y, dir, h), Height(x, y, h),
                            SightLine(p, q, dir, other), if other < h.
        };
        query heightData, lp select (x, y) from Visible(x, y) |> Vector.length
    )
}

// Extra visualization function
def _show(maxX: Int32, maxY: Int32, l: List[(Int32, Int32)]): String = region r {
    use StringBuilder.append!;
    let sb = StringBuilder.new(r);
    let visible = List.toSet(l);
    foreach(x <- List.range(0, maxX); y <- List.range(0, maxY)) {
        if (y == 0) append!(String.lineSeparator(), sb) else ();
        let str = if (Set.exists(Eq.eq((x, y)), visible)) "#" else ".";
        append!(str, sb)
    };
    sb |> StringBuilder.toString
}

def solve2(_lines: List[String]): Int32 = 42

// =============================================================================
// imperative solution
// =============================================================================

// def crash(opt: Option[t]): t = match opt {
//     case None => ???
//     case Some(v) => v
// }

// def intList(l: String): List[Int32] = {
//     l |> String.toList |> map(
//         Char.toString >> Int32.fromString >> crash
//     )
// }

// def boolToInt(b: Bool): Int32 = if (b) 1 else 0

// def solve1(lines: List[String]): Int32 = region r {
//     let len2 = lines |> List.head |> map(String.length) |> crash;
//     let trees = lines |>
//                 map(intList >> List.toArray(r)) |>
//                 List.toArray(r);
//     let len1 = Array.length(trees);
//     let visible = Array.init(r, _ -> Array.init(r, _ -> false, len2), len1);
//     foreach(x <- Iterator.range(r, 0, len1)) {
//         // horizontal sweeps
//         let leftMax = ref 0;
//         let rightMax = ref 0;
//         foreach(y <- Iterator.range(r, 0, len2)) {
//             if (y == 0 or y == len2-1) {
//                 visible[x][y] = true;
//                 leftMax := trees[x][y];
//                 rightMax := trees[x][len2-1-y]
//             } else {
//                 let left = trees[x][y];
//                 if (left > deref leftMax) {
//                     visible[x][y] = true;
//                     leftMax := left
//                 } else ();

//                 let right = trees[x][len2-1-y];
//                 if (right > deref rightMax) {
//                     visible[x][len2-1-y] = true;
//                     rightMax := right
//                 } else ()
//             }
//         }
//     };
//     foreach(y <- Iterator.range(r, 0, len2)) region local {
//         // vertical sweeps
//         let topMax = ref 0;
//         let botMax = ref 0;
//         foreach(x <- Iterator.range(r, 0, len1)) {
//             if (x == 0 or x == len1-1) {
//                 visible[x][y] = true;
//                 topMax := trees[x][y];
//                 botMax := trees[len1-1-x][y]
//             } else {
//                 let top = trees[x][y];
//                 if (top > deref topMax) {
//                     visible[x][y] = true;
//                     topMax := top
//                 } else ();

//                 let bot = trees[len1-1-x][y];
//                 if (bot > deref botMax) {
//                     visible[len1-1-x][y] = true;
//                     botMax := bot
//                 } else ()
//             }
//         }
//     };
//     visible |> Array.map(Array.map(boolToInt) >> Array.sum) |> Array.sum
// }

}
