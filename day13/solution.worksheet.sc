import scala.annotation.tailrec

val rawInput = scala.io.Source.fromFile("./day13/input.txt").mkString
val problems = rawInput.split(sys.props("line.separator") ++ sys.props("line.separator")).toList

enum Cell:
  case Ash
  case Rock

case class Coord(x: Int, y: Int)

def parseProblem(problem: String): Map[Coord, Cell] = {
  val lines = problem.split(sys.props("line.separator")).toArray
  var map = Map.empty[Coord, Cell]

  for {
    y <- 0 until lines.length
    x <- 0 until lines(y).length
  } {
    lines(y)(x) match {
      case '.' => map += Coord(x, y) -> Cell.Ash
      case '#' => map += Coord(x, y) -> Cell.Rock
      case c => throw new Exception("Invalid input: " + c)
    }
  }

  map
}

trait Stepper:
  def apply(coord: Coord): Coord
  def inverse(): Stepper

def Up: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x, y = coord.y - 1)
  def inverse(): Stepper = Down

def Down: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x, y = coord.y + 1)
  def inverse(): Stepper = Up

def Left: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x - 1, y = coord.y)
  def inverse(): Stepper = Right

def Right: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x + 1, y = coord.y)
  def inverse(): Stepper = Left

def isReflectionOnLine(coord: Coord, stepper: Stepper, map: Map[Coord, Cell]): Boolean =
  var right = stepper(coord)
  var left = coord
  val reverse = stepper.inverse()

  if (!map.contains(right)) return false

  while (map.contains(right) && map.get(left) == map.get(right)) {
    right = stepper(right)
    left = reverse(left)
  }

  map.get(left) -> map.get(right) match {
    case (Some(a), Some(b)) if a != b => false
    case _ => true
  }

def findReflection(findStepper: Stepper, rowStepper: Stepper, map: Map[Coord, Cell]): List[Coord] = {
  var coord = Coord(0, 0)
  var result: List[Coord] = Nil
  while (map.contains(coord)) {
    var row = coord
    var found = true
    while (map.contains(row) && found) {
      if (!isReflectionOnLine(row, findStepper, map)) {
        found = false
      }
      row = rowStepper(row)
    }
    coord = findStepper(coord)
    if (found) {
      result = coord :: result
    }
  }

  result
}

def findReflections(map: Map[Coord, Cell]): List[Coord] = {
  val vertical = findReflection(Down, Right, map)
  val horizontal = findReflection(Right, Down, map)

  vertical ++ horizontal
}

def scoreReflection(coord: Coord): Int = coord.y * 100 + coord.x

// val test1 = parseProblem(problems.head)
// val test2 = findReflection(Down, Down, test1)

val part1 = problems.map(parseProblem).map(findReflections).flatten.map(scoreReflection).sum

def showMap(map: Map[Coord, Cell]): String = {
  val maxX = map.keys.map(_.x).max
  val maxY = map.keys.map(_.y).max
  val sb = new StringBuilder()

  for {
    y <- 0 to maxY
    x <- 0 to maxX
  } {
    val coord = Coord(x, y)
    sb.append(map.get(coord) match {
      case Some(Cell.Ash) => '.'
      case Some(Cell.Rock) => '#'
      case None => ' '
    })
    if (x == maxX) sb.append(sys.props("line.separator"))
  }

  sb.toString()
}

def findAlternative(map: Map[Coord, Cell]): Coord = {
  val defaultReflection = findReflections(map).head

  val results = for {
    k <- map.keys
  } yield {
    val alternative = map(k) match {
      case Cell.Ash => Cell.Rock
      case Cell.Rock => Cell.Ash
    }
    val alternativeMap = map + (k -> alternative)
    val reflections = findReflections(alternativeMap).filter(_ != defaultReflection)
    if (reflections.length == 1) {
      reflections
    } else {
      Nil
    }
  }

  val flattened = results.flatten.toList
  val len = flattened.length
  len match {
    case 0 => throw new Exception("No alternative found for: " + showMap(map))
    case 1 => flattened.head
    case _ => throw new Exception("Multiple alternatives found: " + flattened)
  }
}

val part2 = problems.map(parseProblem).map(findAlternative).map(scoreReflection).sum