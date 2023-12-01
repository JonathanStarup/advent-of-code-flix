mod Util {
use Result.flatMap

pub def solveFile(
    input: String, output: String, f: List[String] -> t \ ef
): Result[IOError, Unit] \ {IO, ef} with ToString[t] = {
    (Files.readLines(input)) |> Result.flatMap( lines -> {
        println("Solving file ${input}:");

        let solution = f(lines);

        println("${solution}\n");
        for (
            _ <- Files.deleteIfExists(output);
            res <- writeWithUnit(output, solution)
        ) yield res
    })
}

def writeWithUnit(file: String, content: t): Result[IOError, Unit] \ IO with ToString[t] = {
    Files.write(file, content) |> Result.map(_ -> ())
}
}
