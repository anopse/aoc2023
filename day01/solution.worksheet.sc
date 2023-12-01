
val rawInput = scala.io.Source.fromFile("./day01/input.txt").mkString

val part1ValueMap = (0 to 9).map(i => (i.toString, i)).toMap
val part2ValueMap = part1ValueMap ++ Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
)

def parseInput(input: String, valueMap: Map[String, Int]): List[List[Int]] = {
  for {
    line <- input.split("\n").toList
    if line.nonEmpty
  } yield {
    for {
        i <- line.indices.toList
        skipped = line.drop(i)
        matching <- valueMap.view.filterKeys(skipped.startsWith)
        value = matching._2
    } yield value
  }
}

def solveLine(line: List[Int]): Int = {
    val first = line.head
    val last = line.last
    first * 10 + last
}

def solve(input: String, valueMap: Map[String, Int]): Int = {
  val parsedInput = parseInput(input, valueMap)
  val solvedLines = parsedInput.map(solveLine)
  solvedLines.sum
}

println(solve(rawInput, part1ValueMap))
println(solve(rawInput, part2ValueMap))