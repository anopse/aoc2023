import scala.annotation.tailrec
val rawInput = scala.io.Source.fromFile("./day12/input.txt").mkString
val lines = rawInput.split(sys.props("line.separator")).toList

enum Cell:
  case Operational
  case Damaged
  case Unknown

case class Line(
  cells: Array[Cell],
  clues: Array[Int]
)

def parseLines(lines: List[String]): List[Line] = {
  lines.map { line =>
    val splitted = line.split(" ").filter(_.nonEmpty)
    val cellPart = splitted(0)
    val cluePart = splitted(1)
    val cells = cellPart.map { char =>
      char match {
        case '.' => Cell.Operational
        case '#' => Cell.Damaged
        case '?' => Cell.Unknown
        case _ => throw Exception(s"Invalid char $char")
      }
    }.toArray

    val clues = cluePart.split(",").map(_.toInt).toArray

    Line(cells, clues)
  }
}

val input = parseLines(lines)

def takeCombination[A](list: List[A], n: Int): LazyList[List[A]] = {
  if (n == 0) {
    LazyList(Nil)
  } else {
    list match {
      case Nil => LazyList.empty
      case head :: tail =>
        for {
          appendedHead <- LazyList(List(head), Nil)
          tailCombination <- takeCombination(tail, n - 1)
        } yield (appendedHead ++ tailCombination)
    }
  }
}

// val test1 = takeCombination(List(1, 2, 3, 4, 5, 3, 2, 1, 4, 6, 3, 2, 1, 4, 6, 3, 2, 1, 6, 4, 3, 6 ,4), 8)

def allPossibilities(line: Line): Iterator[Line] = {
  val clues = line.clues
  val cells = line.cells

  val cluesSum = clues.sum
  val cellsSum = cells.count(_ == Cell.Damaged)
  val lacking = cluesSum - cellsSum
  val combinations = takeCombination(cells.zipWithIndex.filter(_._1 == Cell.Unknown).map(_._2).toList, lacking).iterator
  val templateCells = cells.map {
    case Cell.Unknown => Cell.Operational
    case other => other
  }

  combinations.map { damaged =>
    val newCells = templateCells.clone()
    damaged.foreach(i => newCells(i) = Cell.Damaged)
    Line(newCells, clues)
  }
}

// for {
//   p <- allPossibilities(Line(Array(Cell.Unknown, Cell.Operational, Cell.Damaged), Array(1)))
// } {
//   println(p.cells.toList)
// }

@tailrec
final def checkLineValid(remainingCell: List[Cell], remainingClue: List[Int]): Boolean = {
  //println(s"remainingCell: $remainingCell, remainingClue: $remainingClue")
  remainingCell match {
    case Nil => remainingClue.isEmpty
    case Cell.Operational :: tail =>
      checkLineValid(tail, remainingClue)
    case Cell.Damaged :: tail =>
      remainingClue match {
        case Nil => false
        case head :: clueTail =>
          if (head == 1 && tail.headOption.contains(Cell.Damaged)) {
            false
          } else {
            val newClueHead = head - 1
            if (newClueHead >= 1 && tail.headOption.contains(Cell.Operational)) {
              false
            } else {
              if (newClueHead == 0) {
                checkLineValid(tail, clueTail)
              } else {
                checkLineValid(tail, newClueHead +: clueTail)
              }
            }
          }
      }
    case _ => throw Exception("Invalid cell")
  }
}

def isLineValid(line: Line): Boolean = {
  checkLineValid(line.cells.toList, line.clues.toList)
}

def getValidPossibilities(line: Line): Iterator[Line] = {
  allPossibilities(line).filter(isLineValid)
}

val possibilities = input.map(l => getValidPossibilities(l).size).sum

def multiplyList[A](list: List[A], times: Int): List[A] = {
  for {
    _ <- (1 to times).toList
    item <- list
  } yield item
}

val part2Input = input.map { line =>
  val newCells = multiplyList(line.cells.toList, 5)
  val newClues = multiplyList(line.clues.toList, 5)
  line.copy(cells = newCells.toArray, clues = newClues.toArray)
}

//val part2Possibilities = part2Input.take(1).map(l => getValidPossibilities(l).size.toLong).sum