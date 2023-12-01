
val rawInput = scala.io.Source.fromFile("./day01/input.txt").mkString

// Part 1

def parseInput(input: String): List[List[Int]] = {
  val lines = input.split("\n")

  for {
    line <- lines.toList
    // if line.nonEmpty
  } yield {
    val onlyDigits = line.filter(_.isDigit)
    onlyDigits.map(_.asDigit).toList
  }
}

def solveLine(line: List[Int]): Int = {
    val first = line.head
    val last = line.last
    first * 10 + last
}

def solvePart1(input: String): Int = {
  val parsedInput = parseInput(input)
  val solvedLines = parsedInput.map(solveLine)
  solvedLines.sum
}

//println(solvePart1(rawInput))

// Part 2

val replacementMap = Map(
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

def findEarliest(line: String): Int = {
    var index = 0

    while (index < line.length) {
        val char = line(index)
        if (char.isDigit) {
            return char.asDigit
        } else {
            val skipped = line.drop(index)
            for {
                (word, replacement) <- replacementMap
            } {
                if (skipped.startsWith(word)) {
                    // todo: avoid non-local return
                    return replacement
                }
            }
        }

        index += 1
    }

    throw new Exception("No digit found")
}

def findLatest(line: String): Int = {
    var index = line.length - 1

    while (index >= 0) {
        val char = line(index)
        if (char.isDigit) {
            return char.asDigit
        } else {
            val skipped = line.drop(index)
            for {
                (word, replacement) <- replacementMap
            } {
                if (skipped.startsWith(word)) {
                    // todo: avoid non-local return
                    return replacement
                }
            }
        }
        index -= 1
    }

    throw new Exception("No digit found")
}

def parseInput2(input: String): List[List[Int]] = {
    for {
        line <- input.split("\n").toList
    } yield {
        val earliest = findEarliest(line)
        val latest = findLatest(line)
        List(earliest, latest)
    }
}

def solvePart2(input: String): Int = {
  val parsedInput = parseInput2(input)
  //println(parsedInput)
  val solvedLines = parsedInput.map(solveLine)
  solvedLines.sum
}

println(solvePart2(rawInput))