mod Y2023.Day01.Solution {
    use Direction.{Left, Right}
    use ToString.toString
    use Functor.map
    use Eq.eq

    pub def main(): Unit \ IO = {
        Util.solveDayRes(2023, 1, calibrationNumber(numericDigits()));
        Util.solveDayRes(2023, 1, calibrationNumber(numericDigits() `Map.union` letterDigits()))
    }

    def calibrationNumber(digits: Digits, lines: List[String]): Result[String, Int32] = {
        lines |> map(singleCalibrationNumber(digits)) |> Result.sequence |> map(List.sum)
    }

    type alias Digits = Map[String, Int32]

    def singleCalibrationNumber(digits: Digits, s: String): Result[String, Int32] = {
        for (
            firstDigit <- s |> getDigit(digits, Left);
            lastDigit <- s |> getDigit(digits, Right);
            number <- combineDigits(firstDigit, lastDigit)
        ) yield number
    }

    def combineDigits(x: Int32, y: Int32): Result[String, Int32] = {
        if (x < 1 or x > 9 or y < 1 or y > 9) Err("Cannot combine ${x} and ${y}")
        else Ok(x * 10 + y)
    }

    enum Direction with Eq { case Left, Right }

    /// Workaround since `Regex.getLast` is buggy (https://github.com/flix/flix/issues/6792#issuecomment-1836305614)
    def getDigit(digits: Digits, d: Direction, s: String): Result[String, Int32] = {
        let reverseIfRight = if (d == Right) String.reverse else identity;
        let stringToSearch = reverseIfRight(s);
        let digitWords = digits |> Map.keysOf |> Set.toList |> map(reverseIfRight);
        for (
            regex <- acceptAny(digitWords);
            digitString <- Regex.getFirst(substr = regex, stringToSearch) |> Util.toOk(_ -> "no digits in ${s}");
            digit <- digitFromString(digits, reverseIfRight(digitString))
        ) yield digit
    }

    def numericDigits(): Digits = {
        List.range(1, 10) |> map(i -> (toString(i), i)) |> List.toMap
    }

    def letterDigits(): Digits = List#{
        ("one"  , 1),
        ("two"  , 2),
        ("three", 3),
        ("four" , 4),
        ("five" , 5),
        ("six"  , 6),
        ("seven", 7),
        ("eight", 8),
        ("nine" , 9)
    } |> List.toMap

    def acceptAny(l: List[String]): Result[String, Regex] = {
        l |> List.join("|") |> String.toRegex
    }

    def digitFromString(digits: Digits, s: String): Result[String, Int32] = {
        digits |> Map.get(s) |> Util.toOk(_ -> "${s} is not a digit")
    }

}
