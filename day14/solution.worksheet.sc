import scala.annotation.tailrec

val rawInput = scala.io.Source.fromFile("./day14/input.txt").mkString

enum Cell:
  case RoundRock
  case CubeRock
  case Empty

case class Coord(x: Int, y: Int)

def parseProblem(problem: String): Map[Coord, Cell] = {
  val lines = problem.split(sys.props("line.separator")).toArray
  var map = Map.empty[Coord, Cell]

  for {
    y <- 0 until lines.length
    x <- 0 until lines(y).length
  } {
    lines(y)(x) match {
      case '.' => map += Coord(x, y) -> Cell.Empty
      case '#' => map += Coord(x, y) -> Cell.CubeRock
      case 'O' => map += Coord(x, y) -> Cell.RoundRock
      case c => throw new Exception("Invalid input: " + c)
    }
  }

  map
}

val problem = parseProblem(rawInput)

trait Stepper:
  def apply(coord: Coord): Coord
  def inverse(): Stepper
  def ord(): Coord => Int

def Up: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x, y = coord.y - 1)
  def inverse(): Stepper = Down
  def ord(): Coord => Int = _.y

def Down: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x, y = coord.y + 1)
  def inverse(): Stepper = Up
  def ord(): Coord => Int = -_.y

def Left: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x - 1, y = coord.y)
  def inverse(): Stepper = Right
  def ord(): Coord => Int = _.x

def Right: Stepper = new Stepper:
  def apply(coord: Coord): Coord = Coord(x = coord.x + 1, y = coord.y)
  def inverse(): Stepper = Left
  def ord(): Coord => Int = -_.x

def moveRounds(map: Map[Coord, Cell], stepper: Stepper): Map[Coord, Cell] = {
  val rounds = map.filter(_._2 == Cell.RoundRock).keys.toList.sortBy(stepper.ord())
  var newMap = map
  val inverseStepper = stepper.inverse()

  rounds.foreach(round => {
    var newRound = stepper(round)
    while (newMap.contains(newRound) && newMap(newRound) == Cell.Empty) {
      newRound = stepper(newRound)
    }
    newRound = inverseStepper(newRound)
    newMap += round -> Cell.Empty
    newMap += newRound -> Cell.RoundRock
    // println(s"Moving round from $round to $newRound")
  })

  newMap
}

val mapAfterTrigger = moveRounds(problem, Up)

def printMap(map: Map[Coord, Cell]): Unit = {
  val maxX = map.keys.maxBy(_.x).x
  val maxY = map.keys.maxBy(_.y).y

  for {
    y <- 0 to maxY
    x <- 0 to maxX
  } {
    val coord = Coord(x, y)
    print(map.get(coord) match {
      case Some(Cell.RoundRock) => 'O'
      case Some(Cell.CubeRock) => '#'
      case Some(Cell.Empty) => '.'
      case None => ' '
    })
    if (x == maxX) println()
  }
}

// printMap(problem)
//printMap(mapAfterTrigger)

def scoreCubes(map: Map[Coord, Cell]): Int = {
  val maxY = map.keys.maxBy(_.y).y + 1
  map.filter(_._2 == Cell.RoundRock).keys.toList.map(maxY - _.y).sum
}

// val roundRocks = mapAfterTrigger.filter(_._2 == Cell.RoundRock).keys.toList.sortBy(_.y)

val part1 = scoreCubes(mapAfterTrigger)

val cycleStepper = List(Up, Left, Down, Right)

def keyOf(map: Map[Coord, Cell]): Set[Coord] = {
  map.filter(_._2 == Cell.RoundRock).keys.toSet
}

def memoize[I, K, O](f: I => O, fk: I => K): I => O = {
  val hashMap = new scala.collection.mutable.HashMap[K, O]()

  ((input: I) => hashMap.getOrElseUpdate(fk(input), f(input)))
}

def do1000Times[V](f: V => V, input: V): V = {
  var result = input
  for {
    i <- 1 to 1000
  } {
    result = f(result)
  }
  result
}

def memoDo1000Times[V, K](f: V => V, fk: V => K): V => V = {
  memoize((input: V) => do1000Times(f, input), fk)
}

def moves(map: Map[Coord, Cell]): Map[Coord, Cell] = {
  cycleStepper.foldLeft(map)((map, stepper) => moveRounds(map, stepper))
}

def memoMoves(map: Map[Coord, Cell]): Map[Coord, Cell] = {
  memoize(moves, keyOf)(map)
}

val move1kTimes = memoDo1000Times(memoMoves, keyOf)
val move1mTimes = memoDo1000Times(move1kTimes, keyOf)
val move1bTimes = memoDo1000Times(move1mTimes, keyOf)

val result = move1bTimes(problem)
printMap(result)
println(scoreCubes(result))