import scala.annotation.tailrec

val rawInput = scala.io.Source.fromFile("./day21/input.txt").mkString

enum Cell:
  case Empty
  case Rock

case class Coord(x: Int, y: Int)

type CellMap = Map[Coord, Cell]

case class Input(map: CellMap, start: Coord)

def parseProblem(problem: String): Input = {
  val lines = problem.split(sys.props("line.separator")).toArray
  var map = Map.empty[Coord, Cell]
  var start = Coord(-1, -1)

  for {
    y <- 0 until lines.length
    x <- 0 until lines(y).length
  } {
    val cell = lines(y)(x) match {
      case '.' => Cell.Empty
      case '#' => Cell.Rock
      case 'S' => 
        start = Coord(x, y)
        Cell.Empty
      case c => throw new Exception("Invalid input: " + c)
    }
    map += Coord(x, y) -> cell
  }

  Input(map, start)
}

val input = parseProblem(rawInput)

trait Stepper:
  def apply(coord: Coord): Coord

val Up: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x, y = coord.y - 1)

val Down: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x, y = coord.y + 1)

val Left: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x - 1, y = coord.y)

val Right: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x + 1, y = coord.y)

val allSteppers = List(Up, Down, Left, Right)

case class MapInfo(maxX: Int, maxY: Int, infinite: Boolean)

def propagate(location: Coord, map: CellMap, mapInfo: MapInfo): List[Coord] =
  val all = allSteppers.map { stepper =>
    stepper(location)
  }
  
  mapInfo.infinite match {
    case false =>
      all.filter { coord =>
        map.get(coord).contains(Cell.Empty)
      }
    case true =>
      val modX = mapInfo.maxX + 1
      val modY = mapInfo.maxY + 1
      all.filter { coord =>
        val x = Math.floorMod(coord.x, modX)
        val y = Math.floorMod(coord.y, modY)
        map(Coord(x, y)) == Cell.Empty
      }
  }
  

def propagateAll(locations: Set[Coord], map: CellMap, mapInfo: MapInfo): Set[Coord] =
  locations.flatMap { location =>
    propagate(location, map, mapInfo)
  }

@tailrec
final def propagateNTimes(n: Int, locations: Set[Coord], map: CellMap, mapInfo: MapInfo): Set[Coord] =
  n match {
    case 0 => locations
    case _ =>
      val newLocations = propagateAll(locations, map, mapInfo)
      propagateNTimes(n - 1, newLocations, map, mapInfo)
  }

def computeMapInfo(map: CellMap): MapInfo =
  val maxX = map.keys.map(_.x).max
  val maxY = map.keys.map(_.y).max

  MapInfo(maxX, maxY, false)

def printMap(map: CellMap, positions: Set[Coord]): Unit =
  val maxX = map.keys.map(_.x).max
  val maxY = map.keys.map(_.y).max

  for {
    y <- 0 to maxY
    x <- 0 to maxX
  } {
    val coord = Coord(x, y)

    val c = map(coord)
    c match
      case Cell.Rock => print("#")
      case Cell.Empty =>
        if positions.contains(coord) then
          print("O")
        else
          print(".")
    
    if x == maxX then println()
  }

val start = input.start
val map = input.map

printMap(map, Set(start))

val mapInfo = computeMapInfo(map)
val infiniteMapInfo = mapInfo.copy(infinite = true)

def after(n: Int) = propagateNTimes(n, Set(start), map, mapInfo)
val size = after(100).size

def afterInfinite(n: Int, from: Set[Coord]) = propagateNTimes(n, from, map, infiniteMapInfo)
//val sizeInfinite = afterInfinite(100, Set(start)).size

def solvePart2() = {
  val period = 131 * 2
  val s0 = afterInfinite(65, Set(start))
  val s1 = afterInfinite(period, s0)
  val s2 = afterInfinite(period, s1)
  val a0 = s0.size
  val a1 = s1.size
  val a2 = s2.size
  val b0 = BigInt(a0)
  val b1 = BigInt(a1 - a0)
  val b2 = BigInt(a2 - a1)
  val n = 26501300 / period
  val result = b0 + b1 * n + (n * (n - 1) / 2) * (b2 - b1)

  def f(x: BigInt): BigInt = {
    b0 * (x - 1) * (x - 2) / 2 +
    b1 * x * (x - 2) * -1 +
    b2 * x * (x - 1) / 2
  }

  f(BigInt(n))
}

val part2 = solvePart2()