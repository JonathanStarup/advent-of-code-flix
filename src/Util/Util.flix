mod Util {
    use ToString.toString

    pub def solveDay(year: Int32, day: Int32, solver: List[String] -> res \ r): Unit \ {IO, r} with ToString[res] = {
        solveDayRes(year, day, solver >> Ok)
    }

    pub def solveDayRes(year: Int32, day: Int32, solver: List[String] -> Result[String, res] \ r): Unit \ {IO, r} with ToString[res] = {
        let yearString = toString(year);
        let dayString = "Day${toString(day) |> String.padLeft(2, '0')}";
        let inputFile = "src" / yearString / dayString / "input.txt";
        println("\n------ ${yearString} ${dayString} ------");
        solveFileRes(inputFile, solver);
        println("")
    }

    pub def solveFile(file: String, solver: List[String] -> res \ r): Unit \ {IO, r} with ToString[res] = {
        solveFileRes(file, solver >> Ok)
    }

    pub def solveFileRes(file: String, solver: List[String] -> Result[String, res] \ r): Unit \ {IO, r} with ToString[res] = {
        let res = for (
            inputLines <- Files.readLines(file) |> Result.mapErr(toString);
            result <- solver(inputLines)
        ) yield result;
        match res {
            case Err(msg) => printError(msg)
            case Ok(result) => printSuccess(toString(result))
        }
    }

    def printError(msg: String): Unit \ IO = {
        println("--- An error ocurred ---");
        println(msg);
        println("------------------------")
    }

    def printSuccess(result: String): Unit \ IO = {
        println("------- Success! -------");
        println(result)
    }

    pub def toOk(e: Unit -> e, o: Option[a]): Result[e, a] = match o {
        case None => Err(e())
        case Some(v) => Ok(v)
    }
}
