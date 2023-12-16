import scala.annotation.tailrec

val rawInput = scala.io.Source.fromFile("./day16/input.txt").mkString

enum Cell:
  case Empty
  case Vertical
  case Horizontal
  case DiagonalLeftUp // \
  case DiagonalLeftDown // /

case class Coord(x: Int, y: Int)

type CellMap = Map[Coord, Cell]

def parseProblem(problem: String): CellMap = {
  val lines = problem.split(sys.props("line.separator")).toArray
  var map = Map.empty[Coord, Cell]

  for {
    y <- 0 until lines.length
    x <- 0 until lines(y).length
  } {
    val cell = lines(y)(x) match {
      case '.' => Cell.Empty
      case '|' => Cell.Vertical
      case '-' => Cell.Horizontal
      case '\\' => Cell.DiagonalLeftUp
      case '/' => Cell.DiagonalLeftDown
      case c => throw new Exception("Invalid input: " + c)
    }
    map += Coord(x, y) -> cell
  }

  map
}

trait Stepper:
  def apply(coord: Coord): Coord
  def inverse(): Stepper
  def rotateLeft(): Stepper
  def rotateRight(): Stepper

val Up: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x, y = coord.y - 1)
  def inverse(): Stepper = Down
  def rotateLeft(): Stepper = Left
  def rotateRight(): Stepper = Right
  override def toString(): String = "Up"

val Down: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x, y = coord.y + 1)
  def inverse(): Stepper = Up
  def rotateLeft(): Stepper = Right
  def rotateRight(): Stepper = Left
  override def toString(): String = "Down"

val Left: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x - 1, y = coord.y)
  def inverse(): Stepper = Right
  def rotateLeft(): Stepper = Down
  def rotateRight(): Stepper = Up
  override def toString(): String = "Left"

val Right: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x + 1, y = coord.y)
  def inverse(): Stepper = Left
  def rotateLeft(): Stepper = Up
  def rotateRight(): Stepper = Down
  override def toString(): String = "Right"

case class BeamState(coord: Coord, stepper: Stepper)

def splitBeam(beam: BeamState): List[BeamState] = {
  val left = beam.copy(stepper = beam.stepper.rotateLeft())
  val right = beam.copy(stepper = beam.stepper.rotateRight())
  List(left, right)
}

def move(beam: BeamState, map: CellMap): List[BeamState] = {
  val newCoord = beam.stepper(beam.coord)
  val newBeam = beam.copy(coord = newCoord)
  (map.get(newCoord) -> beam.stepper) match {
    case (None, _) => Nil
    case (Some(Cell.Empty), _) => List(newBeam)
    case (Some(Cell.Vertical), Left | Right) => splitBeam(newBeam)
    case (Some(Cell.Horizontal), Up | Down) => splitBeam(newBeam)
    case (Some(Cell.Vertical), _) => List(newBeam)
    case (Some(Cell.Horizontal), _) => List(newBeam)
    case (Some(Cell.DiagonalLeftUp), Up) => List(newBeam.copy(stepper = Left))
    case (Some(Cell.DiagonalLeftUp), Down) => List(newBeam.copy(stepper = Right))
    case (Some(Cell.DiagonalLeftUp), Left) => List(newBeam.copy(stepper = Up))
    case (Some(Cell.DiagonalLeftUp), Right) => List(newBeam.copy(stepper = Down))
    case (Some(Cell.DiagonalLeftDown), Up) => List(newBeam.copy(stepper = Right))
    case (Some(Cell.DiagonalLeftDown), Down) => List(newBeam.copy(stepper = Left))
    case (Some(Cell.DiagonalLeftDown), Left) => List(newBeam.copy(stepper = Down))
    case (Some(Cell.DiagonalLeftDown), Right) => List(newBeam.copy(stepper = Up))
    case (a, b) => throw new Exception(s"Unexpected input: $a, $b")
  }
}

@tailrec
final def moveAll(beams: List[BeamState], map: CellMap, sideEffectPredicate: BeamState => Boolean): Unit = {
  beams match
    case head :: tail => {
      if (sideEffectPredicate(head)) {
        moveAll(move(head, map) ++ tail, map, sideEffectPredicate)
      } else {
        moveAll(tail, map, sideEffectPredicate)
      }
    }
    case Nil => ()
}

def marker(map: CellMap, start: BeamState): Set[Coord] = {
  var set = Set.empty[BeamState]
  val actualStartCoord = start.stepper.inverse()(start.coord)
  val actualStart = BeamState(actualStartCoord, start.stepper)
  moveAll(List(actualStart), map, beam => {
    if (set.contains(beam)) {
      false
    } else {
      set += beam
      true
    }
  })

  set.map(_.coord) - actualStartCoord
}

val map = parseProblem(rawInput)
val marked = marker(map, BeamState(Coord(0, 0), Down))
val part1 = marked.size


val maxX = map.keys.map(_.x).max
val maxY = map.keys.map(_.y).max

val possibleStarts: List[BeamState] = {
  val top = (0 to maxX).map(x => BeamState(Coord(x, 0), Down)).toList
  val bottom = (0 to maxX).map(x => BeamState(Coord(x, maxY), Up)).toList
  val left = (0 to maxY).map(y => BeamState(Coord(0, y), Right)).toList
  val right = (0 to maxY).map(y => BeamState(Coord(maxX, y), Left)).toList

  top ++ bottom ++ left ++ right
}

val part2 = possibleStarts.map(marker(map, _).size).max