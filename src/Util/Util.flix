namespace Util {
    use Result.flatMap

    pub def solveFile(
        input: String, output: String, f: List[String] -> t \ ef
    ): Result[Unit, String] \ {IO, ef} with ToString[t] = {
        let* lines = File.readLines(input);

        println("Solving file ${input}:");

        let solution = f(lines);

        println("${solution}\n");
        let* _ = File.deleteIfExists(output);
        writeWithUnit(output, solution)
    }

    def writeWithUnit(file: String, content: t): Result[Unit, String] \ IO with ToString[t] = {
        let* written = File.write(file, content);
        if (written) Ok() else Err("Could not write file.")
    }

}
