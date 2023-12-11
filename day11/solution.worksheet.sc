val rawInput = scala.io.Source.fromFile("./day11/input.txt").mkString
val lines = rawInput.split(sys.props("line.separator")).toList

type Coord = (Int, Int)

def parseLines(lines: List[String]): Set[Coord] = {
  lines.zipWithIndex.flatMap { (line, y) =>
    line.zipWithIndex.flatMap { (char, x) =>
      char match {
        case '#' => Some((x, y))
        case '.' => None
        case _ => throw Exception(s"Invalid char $char")
      }
    }
  }.toSet
}

val galaxies = parseLines(lines)

val galaxiesbyX = galaxies.groupBy(_._1)
val galaxiesbyY = galaxies.groupBy(_._2)

def distanceBetweenGalaxies(a: Coord, b: Coord): Long = {
  var d = 0L
  val minX = Math.min(a._1, b._1)
  val maxX = Math.max(a._1, b._1)
  for {
    x <- minX until maxX
  } {
    if (galaxiesbyX.get(x).isDefined) {
      d += 1L
    } else {
      d += 2L
    }
  }

  val minY = Math.min(a._2, b._2)
  val maxY = Math.max(a._2, b._2)

  for {
    y <- minY until maxY
  } {
    if (galaxiesbyY.get(y).isDefined) {
      d += 1L
    } else {
      d += 2L
    }
  }

  d
}

val allDistances = {
  for {
    a <- galaxies.toList
    b <- galaxies.toList
  } yield (a, b) -> distanceBetweenGalaxies(a, b)
}.toMap

val part1 = allDistances.values.sum / 2

val emptyDist = 1000000L

def distanceBetweenGalaxiesPart2(a: Coord, b: Coord): Long = {
  var d = 0L
  val minX = Math.min(a._1, b._1)
  val maxX = Math.max(a._1, b._1)
  for {
    x <- minX until maxX
  } {
    if (galaxiesbyX.get(x).isDefined) {
      d += 1L
    } else {
      d += emptyDist
    }
  }

  val minY = Math.min(a._2, b._2)
  val maxY = Math.max(a._2, b._2)

  for {
    y <- minY until maxY
  } {
    if (galaxiesbyY.get(y).isDefined) {
      d += 1L
    } else {
      d += emptyDist
    }
  }

  d
}

val allDistancesPart2 = {
  for {
    a <- galaxies.toList
    b <- galaxies.toList
  } yield (a, b) -> distanceBetweenGalaxiesPart2(a, b)
}.toMap


val part2 = allDistancesPart2.values.sum / 2