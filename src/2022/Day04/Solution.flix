mod Y2022.Day04.Solution {
use Functor.map
use Util.solveDay
use Result.flatMap

pub def main(): Unit \ IO = {
    solveDay(2022, 4, solve1);
    solveDay(2022, 4, solve2)
}

def forceIntoTuple(l: List[a]): (a, a) = match l {
    case one :: two :: Nil => (one, two)
    case _ => ???
}

def forceOption(o: Option[a]): a = match o {
    case None => ???
    case Some(v) => v
}

def boolToInt32(b: Bool): Int32 = if (b) 1 else 0

type alias Range = (Int32, Int32)

def lineToRanges(line: String): (Range, Range) = {
    let splitOnComma = String.split(regex = ",");
    let stringToRange =
        String.split(regex = "-") >>
        map(Int32.fromString >> forceOption) >>
        forceIntoTuple;
    let lineToOverlap =
        splitOnComma >>
        map(stringToRange) >>
        forceIntoTuple;
    lineToOverlap(line)
}

def solve1(lines: List[String]): Int32 = {
    let overlappingRanges = match (a, b) -> {
        overlap(a, b) |> boolToInt32
    };
    lines |> map(lineToRanges >> overlappingRanges) |> List.sum
}

def solve2(lines: List[String]): Int32 = {
    let touchingRanges = match (a, b) -> {
        (not disjoint(a, b)) |> boolToInt32
    };
    lines |> map(lineToRanges >> touchingRanges) |> List.sum
}

def overlap(range1: Range, range2: Range): Bool = {
    subsumedBy(range1, range2) or subsumedBy(range2, range1)
}

def subsumedBy(range1: Range, range2: Range): Bool = {
    let (r1l, r1h) = range1;
    let (r2l, r2h) = range2;
    r1l >= r2l and r1h <= r2h
}

def disjoint(range1: Range, range2: Range): Bool = {
    let (r1l, r1h) = range1;
    let (r2l, r2h) = range2;
    r1h < r2l or r2h < r1l
}

}
