import scala.annotation.tailrec

val rawInput = scala.io.Source.fromFile("./day17/input.txt").mkString

case class Coord(x: Int, y: Int)

type CellMap = Map[Coord, Int]

def parseProblem(problem: String): CellMap = {
  val lines = problem.split(sys.props("line.separator")).toArray
  var map = Map.empty[Coord, Int]

  for {
    y <- 0 until lines.length
    x <- 0 until lines(y).length
  } {
    val cell = lines(y)(x) match {
      case x if x.isDigit => x.toString().toInt
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

val Directions = List(Up, Down, Left, Right)

val map = parseProblem(rawInput)

val bigDist = 999999

def costFunction(coord: Coord): Int = map.getOrElse(coord, bigDist)

case class State(coord: Coord, cost: Int, prev: List[Coord])

def nTh[A](list: List[A], n: Int): Option[A] = {
  @tailrec
  def loop(list: List[A], n: Int): Option[A] = {
    if (n == 0) {
      list.headOption
    } else {
      list match {
        case Nil => None
        case _ :: tail => loop(tail, n - 1)
      }
    }
  }
  loop(list, n)
}

def straightCount(coords: List[Coord]): Int = {
  if (coords.length <= 2) {
    coords.length
  } else {
    val dx = coords(0).x - coords(1).x
    val dy = coords(0).y - coords(1).y
    if (dx == 0) {
      val x = coords(0).x
      coords.view.takeWhile(_.x == x).size
    } else {
      val y = coords(0).y
      coords.view.takeWhile(_.y == y).size
    }
  }
}

val test = straightCount(List(Coord(0, 0), Coord(0, 1), Coord(1, 2), Coord(1, 3)))

case class CacheKey(coord: Coord, dx: Int, dy: Int, straightFor: Int)

def getCacheKey(coords: List[Coord]): CacheKey = {
  if (coords.length <= 2) {
    CacheKey(coords(0), 0, 0, coords.length)
  } else {
    val dx = coords(0).x - coords(1).x
    val dy = coords(0).y - coords(1).y
    val straightFor = straightCount(coords)
    CacheKey(coords(0), dx, dy, straightFor)
  }
}

def isValid(minDist: Int, maxDist: Int)(state: State): Boolean = {
  // must not go backward
  val prevPrev = nTh(state.prev, 1)
  val currentCoord = state.coord
  val isNotBackward = prevPrev match {
    case Some(prevPrevCoord) => prevPrevCoord != currentCoord
    case None => true
  }

  // must not go maxDist steps in the same direction
  val prev4 = nTh(state.prev, maxDist)
  val dx = currentCoord.x - prev4.map(_.x).getOrElse(currentCoord.x)
  val dy = currentCoord.y - prev4.map(_.y).getOrElse(currentCoord.y)
  val isNotFourSteps = dx.abs <= maxDist && dy.abs <= maxDist

  // cost is less than bigDist
  val isNotTooBig = state.cost < bigDist

  // must go at least minDist steps in the same direction
  val isNotTooShort: Boolean = if (minDist > 0) {
    val list = state.coord::state.prev
    val listLen = state.prev.view.take(minDist).size + 1
    if (listLen > 2) {
      val isATurn = straightCount(list) == 2
      if (!isATurn) {
        true
      } else {
        if (listLen < minDist) {
          false
        } else {
          val wasStraightFor = straightCount(list.tail)
          wasStraightFor >= minDist
        }
      }
    } else {
      true
    }
  } else {
    true
  }
  

  isNotBackward && isNotFourSteps && isNotTooBig && isNotTooShort
}

// val c1 = Coord(bigDist, 1)
// val c2 = Left(c1)
// val c3 = Up(c2)
// val c4 = Up(c3)
// val c5 = Up(c4)

// val s1 = State(c1, 0, None)
// val s2 = State(c2, 1, Some(s1))
// val s3 = State(c3, 2, Some(s2))
// val s4 = State(c4, 3, Some(s3))
// val s5 = State(c5, 4, Some(s4))

// val test = isValid(s5)

def bfs(start: Coord, end: Coord, validationFunction: State => Boolean, minDist: Int): State = {
  val queue = scala.collection.mutable.PriorityQueue.empty[State](Ordering.by(-_.cost))
  var distCache = Map.empty[CacheKey, Int]

  queue.enqueue(State(start, 0, Nil))

  while (queue.nonEmpty) {
    val current = queue.dequeue()
    val currentCoord = current.coord
    val currentCost = current.cost
    val newPrev = currentCoord::current.prev
    //val nextCost = queue.headOption.map(_.cost).getOrElse(currentCost)
    //println(s"currentCost: $currentCost, nextCost: $nextCost, size: ${queue.size}")

    val cacheKey = getCacheKey(newPrev)

    def beatCache = {
      val cacheValue = distCache.getOrElse(cacheKey, bigDist)
      currentCost < cacheValue
    }

    if (currentCoord == end && straightCount(newPrev) >= minDist) {
      return current
    } else if (beatCache) {
      distCache += cacheKey -> currentCost
      val neighbors = Directions.map(d => {
        val newCoord = d(currentCoord)
        val newCost = currentCost + costFunction(newCoord)

        State(newCoord, newCost, newPrev)
      }).filter(validationFunction)

      neighbors.foreach { neighbor =>
        queue.enqueue(neighbor)
      }
    }
  }

  State(start, 0, Nil)
}

val maxX = map.keys.maxBy(_.x).x
val maxY = map.keys.maxBy(_.y).y
val start = Coord(0, 0)
val end = Coord(maxX, maxY)

val part1Validation = isValid(minDist = 0, maxDist = 3)
val part1 = bfs(start, end, part1Validation, 0).cost

val part2Validation = isValid(minDist = 5, maxDist = 10)
val part2 = bfs(start, end, part2Validation, 5).cost
val part2Path = bfs(start, end, part2Validation, 5).prev

// val visited = part2Path.toSet
// 
// for {
//   y <- 0 to maxY
//   x <- 0 to maxX
// } {
//   val coord = Coord(x, y)
//   val cell = coord match {
//     case c if c == start => 'S'
//     case c if c == end => 'E'
//     case c if visited.contains(c) => 'O'
//     case c => map.getOrElse(c, ' ')
//   }
//   print(cell)
//   if (x == maxX) {
//     println()
//   }
// }