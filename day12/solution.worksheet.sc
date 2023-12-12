import $ivy.`org.scala-lang.modules::scala-parallel-collections:1.0.4`
import scala.annotation.tailrec
val rawInput = scala.io.Source.fromFile("./day12/input.txt").mkString
val lines = rawInput.split(sys.props("line.separator")).toList

enum Cell:
  case Operational
  case Damaged
  case Unknown

case class Line(
  cells: List[Cell],
  clues: List[Int]
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
    }.toList

    val clues = cluePart.split(",").map(_.toInt).toList

    Line(cells, clues)
  }
}

val input = parseLines(lines)

case class CacheEntry(
    cells: List[Cell],
    clues: List[Int],
    lackingDamaged: Int,
    remainingUnknown: Int,
    expectDamaged: Boolean,
    expectOperational: Boolean
)

type Cache = scala.collection.mutable.Map[CacheEntry, Option[Long]]

final def recCountValidPossibilities(cells: List[Cell], clues: List[Int], lackingDamaged: Int, remainingUnknown: Int, expectDamaged: Boolean, expectOperational: Boolean, cache: Cache): Option[Long] = {
  cells match {
    case Cell.Operational::tail =>
      if (expectDamaged) {
        None
      } else {
        recCountValidPossibilities(tail, clues, lackingDamaged, remainingUnknown, false, false, cache)
      }
    case Cell.Damaged::tail =>
      clues match
        case head::next => 
          if (expectOperational) {
            None
          } else {
            val newHead = head - 1
            if (newHead == 0) {
              recCountValidPossibilities(tail, next, lackingDamaged - 1, remainingUnknown, false, true, cache)
            } else {
              recCountValidPossibilities(tail, newHead::next, lackingDamaged - 1, remainingUnknown, true, false, cache)
            }
          }
        case Nil =>
          None
    case Cell.Unknown::tail =>
      val newRemainingUnknown = remainingUnknown - 1
      val useCache = true //newRemainingUnknown == 12
      def solve = {
        if (expectDamaged || (lackingDamaged >= remainingUnknown && !expectOperational)) {
          recCountValidPossibilities(Cell.Damaged :: tail, clues, lackingDamaged, newRemainingUnknown, true, false, cache)
        } else if (expectOperational) {
          recCountValidPossibilities(Cell.Operational :: tail, clues, lackingDamaged, newRemainingUnknown, false, true, cache)
        } else {
          val asDamaged = recCountValidPossibilities(Cell.Damaged :: tail, clues, lackingDamaged, newRemainingUnknown, false, false, cache)
          val asOperational = recCountValidPossibilities(Cell.Operational :: tail, clues, lackingDamaged, newRemainingUnknown, false, false, cache)
          if (asDamaged.isDefined || asOperational.isDefined) {
            Some((asDamaged.getOrElse(0L) + asOperational.getOrElse(0L)))
          } else {
            None
          }
        }
      }

      if (useCache) {
        val cacheEntry = CacheEntry(tail, clues, lackingDamaged, remainingUnknown, expectDamaged, expectOperational)
        cache.get(cacheEntry) match {
          case Some(value) => value
          case None =>
            val result = solve
            cache.put(cacheEntry, result)
            result
        }
      } else {
        solve
      }
      
    case Nil => if (clues.isEmpty) Some(1L) else None
  }
}

def countValidPossibilities(line: Line): Long = {
  val clues = line.clues
  val cells = line.cells
  val expectedDamaged = clues.sum
  val damagedCount = cells.count(_ == Cell.Damaged)
  val remainingUnknown = cells.count(_ == Cell.Unknown)
  val lackingDamaged = expectedDamaged - damagedCount
  recCountValidPossibilities(cells, clues, lackingDamaged, remainingUnknown, false, false, scala.collection.mutable.Map.empty).getOrElse(0L)
}

val part1 = input.map(countValidPossibilities).sum

def multiplyInput[A](list: List[A], times: Int, sep: A): List[A] = {
  (1 to times).map(_ => list).reduce((a, b) => a ++ (sep::b))
}

val part2Input = input.map { line =>
  val newCells = multiplyInput(line.cells, 5, Cell.Unknown)
  val newClues = multiplyInput(line.clues.map(Some.apply), 5, None).flatten
  Line(newCells, newClues)
}

import scala.collection.parallel.CollectionConverters._

//val part2 = part2Input.par.map(countValidPossibilities).sum
val part2 = part2Input.map(countValidPossibilities).sum
