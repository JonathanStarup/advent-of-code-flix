mod Day07.Solution {
use Functor.map
use Util.solveFile

pub def main(): Result[IOError, Unit] \ IO = {
    use Result.flatMap;
    let cwd = "src" / "2022" / "Day07";
    for (
        _ <- solveFile(cwd / "input.txt", cwd / "output1.txt", solve1);
        res <- solveFile(cwd / "input.txt", cwd / "output2.txt", solve2)
    ) yield res
}

pub enum Item[r: Region] {
    // items, size
    case Root(MutList[Item[r], r], Ref[Int32, r])
    // name, items, parent, size
    case Folder(String, MutList[Item[r], r], Item[r], Ref[Int32, r])
    // name, parent, size
    case File(String, Item[r], Int32)
}

def size(i: Item[r]): Int32 \ r = match i {
    case Item.Root(_, rootSize) => deref rootSize
    case Item.Folder(_, _, _, folderSize) => deref folderSize
    case Item.File(_, _, fileSize) => fileSize
}

def computeSizes!(r: Region[r], i: Item[r]): Unit \ r = {
    let initSizes = MutList.forEach(computeSizes!(r));
    let sumSizes = MutList.map(r, size) >> MutList.sum;
    match i {
        case Item.Root(items, rootSize) =>
            initSizes(items);
            rootSize := sumSizes(items)
        case Item.Folder(_, items, _, folderSize) =>
            initSizes(items);
            folderSize := sumSizes(items)
        case Item.File(_, _, _) =>
            ()
    }
}

def findSmallFolders(limit: Int32, i: Item[r]): List[Item[r]] \ r = match i {
    case Item.Root(items, rootSize) =>
        let rec = items |> MutList.toList |> List.flatMap(findSmallFolders(limit));
        if (deref rootSize <= limit) i :: rec else rec
    case Item.Folder(_, items, _, folderSize) =>
        let rec = items |> MutList.toList |> List.flatMap(findSmallFolders(limit));
        if (deref folderSize <= limit) i :: rec else rec
    case Item.File(_, _, _) =>
        Nil
}

def newFolder(r: Region[r], name: String, parent: Item[r]): Item[r] \ r =
    Item.Folder(name, MutList.new(r), parent, ref 0 @ r)

def getItems(i: Item[r]): MutList[Item[r], r] = match i {
    case Item.Root(items, _) => items
    case Item.Folder(_, items, _, _) => items
    case Item.File(_, _, _) => ???
}

def getParent(i: Item[r]): Item[r] = match i {
    case Item.Root(_, _) => ???
    case Item.Folder(_, _, parent, _) => parent
    case Item.File(_, parent, _) => ???
}

def getName(i: Item[r]): String = match i {
    case Item.Root(_, _) => "/"
    case Item.Folder(name, _, _, _) => name
    case Item.File(name, _, _) => name
}

def _pretty(r: Region[r], indent: Int32, i: Item[r]): Unit \ IO = {
    let in = String.repeat(indent/2, "| ");
    match i {
        case Item.Root(items, rootSize) =>
            println("${in}- ${getName(i)} (dir, size=${deref rootSize})");
            items |> MutList.sortBy(r, getName) |> MutList.forEach(_pretty(r, indent + 2))
        case Item.Folder(_, items, _, folderSize) =>
            println("${in}- ${getName(i)} (dir, size=${deref folderSize})");
            items |> MutList.forEach(_pretty(r, indent + 2))
        case Item.File(_, _, fileSize) =>
            println("${in}- ${getName(i)} (file, size=${fileSize})")
    }
}

def runTerminal(r: Region[r], lines: List[String], root: Item[r]): Unit \ r = {
    let current = ref root @ r;
    foreach(line <- lines) {
        if (String.isMatch(regex = "\\$ ls", line)) {
            ()
        } else if (String.isMatch(regex = "\\$ cd ..", line)) {
            current := getParent(deref current)
        } else if (String.isMatch(regex = "\\$ cd /", line)) {
            current := root
        } else if (String.isMatch(regex = "\\$ cd [a-zA-Z.]+", line)) {
            let name = String.dropLeft(5, line);
            let items = getItems(deref current);
            match MutList.find(getName >> Eq.eq(name), items) {
                case None => ???
                case Some(dir) => current := dir
            }
        } else if (String.isMatch(regex = "[0-9]+ [a-zA-Z.]+", line)) {
            match String.split(regex = " ", line) {
                case fileSizeStr :: name :: Nil => match Int32.fromString(fileSizeStr) {
                    case Some(fileSize) =>
                        let file = Item.File(name, deref current, fileSize);
                        let items = getItems(deref current);
                        MutList.push!(file, items)
                    case _ => ??? // impossible
                }
                case _ => ??? // impossible
            }
        } else if (String.isMatch(regex = "dir [a-zA-Z.]+", line)) {
            let name = String.dropLeft(4, line);
            let items = getItems(deref current);
            match MutList.find(getName >> Eq.eq(name), items) {
                case None =>
                    let dir = newFolder(r, name, deref current);
                    MutList.push!(dir, items)
                case Some(_) => ???
            }
        } else {
            debug(line);
            ??? // impossible
        }
    }
}

def solve1(lines: List[String]): Int32 = region r {
    let root = Item.Root(MutList.new(r), ref 0 @ r);
    runTerminal(r, lines, root);
    computeSizes!(r, root);
    findSmallFolders(100_000, root) |> map(size) |> List.sum
}

def unwrapFolders(i: Item[r]) : List[Item[r]] \ r = match i {
    case Item.Root(items, _) =>
         i :: (items |> MutList.toList |> List.flatMap(unwrapFolders))
    case Item.Folder(_, items, _, _) =>
        i :: (items |> MutList.toList |> List.flatMap(unwrapFolders))
    case Item.File(_, _, _) => Nil
}

def solve2(lines: List[String]): Int32 = region r {
    let capacity  = 70_000_000;
    let needed = 30_000_000;
    let root = Item.Root(MutList.new(r), ref 0 @ r);
    runTerminal(r, lines, root);
    computeSizes!(r, root);
    let available = capacity - size(root);
    let mustDelete = Int32.max(0, needed - available);
    let sorted = root |> unwrapFolders |> map(i -> (size(i), i)) |> List.sortBy(fst) |> map(snd);
    let deleteOpt = sorted |> List.findLeft(i -> size(i) >= mustDelete);
    match deleteOpt {
        case None => ???
        case Some(delete) => size(delete)
    }
}

}
