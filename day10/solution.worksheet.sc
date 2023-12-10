import scala.annotation.tailrec
val rawInput = scala.io.Source.fromFile("./day10/input.txt").mkString
val lines = rawInput.split(sys.props("line.separator")).toList

enum CellType:
  case NorthSouthPipe
  case EastWestPipe
  case NorthEastPipe
  case NorthWestPipe
  case SouthWestPipe
  case SouthEastPipe
  case Ground
  case Start

val allCellTypes = CellType.values.toList

type CellLocation = (Int, Int)

val charToCellType: Map[Char, CellType] = Map(
  '|' -> CellType.NorthSouthPipe,
  '-' -> CellType.EastWestPipe,
  'L' -> CellType.NorthEastPipe,
  'J' -> CellType.NorthWestPipe,
  '7' -> CellType.SouthWestPipe,
  'F' -> CellType.SouthEastPipe,
  '.' -> CellType.Ground,
  'S' -> CellType.Start
)

val cellTypeToChar: Map[CellType, Char] = charToCellType.map(_.swap)

def parseLines(lines: List[String]): Map[CellLocation, CellType] = {
  lines.zipWithIndex.flatMap { (line, y) =>
    line.zipWithIndex.map { (char, x) =>
      val cellType = charToCellType(char)
      ((x, y), cellType)
    }
  }.toMap
}

val cellTypes = parseLines(lines)

val West = (-1, 0)
val East = (1, 0)
val North = (0, -1)
val South = (0, 1)

val cellTypeToNeighbours: Map[CellType, List[CellLocation]] = Map(
  CellType.NorthSouthPipe -> List(North, South),
  CellType.EastWestPipe -> List(East, West),
  CellType.NorthEastPipe -> List(North, East),
  CellType.NorthWestPipe -> List(North, West),
  CellType.SouthWestPipe -> List(South, West),
  CellType.SouthEastPipe -> List(South, East),
  CellType.Ground -> Nil,
  CellType.Start -> List(North, South, East, West)
)


val neighboursOfCell: Map[CellLocation, List[CellLocation]] = {
  val allNeighbours = cellTypes.map { (cellLocation, cellType) =>
    val neighbours = cellTypeToNeighbours(cellType).map { case (dx, dy) =>
      (cellLocation._1 + dx, cellLocation._2 + dy)
    }.filter(c => cellTypes.contains(c) && cellTypes(c) != CellType.Ground)

    (cellLocation, neighbours)
  }

  // remove incoming neighbours
  allNeighbours.map((cellLocation, neighbours) => 
    (cellLocation, neighbours.filter(n => allNeighbours(n).contains(cellLocation)))
  )
}

def computeDistGraph(from: CellLocation): Map[CellLocation, Int] = {
  @tailrec
  def recComputeDistGraph(toVisit: List[CellLocation], visited: Map[CellLocation, Int]): Map[CellLocation, Int] = {
    if (toVisit.isEmpty) visited
    else {
      val currentCell = toVisit.head 
      val currentDist = visited(currentCell)
      val newDist = currentDist + 1
      val neighbours = neighboursOfCell(currentCell)
      val newNeighbours = neighbours.filter(cell => visited.getOrElse(cell, Int.MaxValue) > newDist)
      val newVisited = visited ++ newNeighbours.map(_ -> newDist)
      val newToVisit = toVisit.tail ++ newNeighbours
      recComputeDistGraph(newToVisit, newVisited)
    }
  }

  recComputeDistGraph(List(from), Map(from -> 0))
}

val start = cellTypes.find(_._2 == CellType.Start).get._1

// might stackoverflow without more stack size, could have done it with trampoline but why not just use -Xss10m for running it once :D

val cellInLoop: Set[CellLocation] = {
  var inLoop = Set(start)
  def recIsCellInLoop(visited: Set[CellLocation], position: CellLocation, firstRec: Boolean): Boolean = {
    val neighbours = neighboursOfCell(position)
    val newNeighbours = neighbours.filterNot(visited.contains)
    var isInLoop = false
    for {
      neighbour <- newNeighbours
    } {
      val newVisited = visited + neighbour
      if (recIsCellInLoop(newVisited, neighbour, false) || (neighbour == start && !firstRec)) isInLoop = true
    }
    if (isInLoop) inLoop += position
    isInLoop
  }

  neighboursOfCell(start).foreach { c =>
    val isInLoop = recIsCellInLoop(Set(c), c, true)
    if (isInLoop) inLoop += c
  }

  inLoop
}

// for {
//   y <- 0 to cellTypes.keys.map(_._2).max
//   x <- 0 to cellTypes.keys.map(_._1).max
//   c = (x, y)
// } {
//   if (x == 0 && y > 0) println()
//   if (cellInLoop.contains(c)) print(cellTypeToChar(cellTypes(c)))
//   else print(".")
// }

val distGraph = computeDistGraph(start)

val part1 = distGraph.filter(kvp => cellInLoop.contains(kvp._1)).values.max

// for {
//   y <- 0 to cellTypes.keys.map(_._2).max
//   x <- 0 to cellTypes.keys.map(_._1).max
// } {
//   val cell = (x, y)
//   val dist = distGraph.getOrElse(cell, -1)
//   if (x == 0 && y > 0) println()
//   if (dist == -1) print(".")
//   else print(dist)
// }

// part 2

// cells map to wall on a 3x3 grid
// example: | cell type (NorthSouthPipe) is :
// 0 1 0
// 0 1 0
// 0 1 0
// so : x = 1, y = 0, 1, 2

val cellToWall: Map[CellType, List[(Int, Int)]] = Map(
  // 0 1 0
  // 0 1 0
  // 0 1 0
  CellType.NorthSouthPipe -> List((1, 0), (1, 1), (1, 2)),
  // 0 0 0
  // 1 1 1
  // 0 0 0
  CellType.EastWestPipe -> List((0, 1), (1, 1), (2, 1)),
  // 0 1 0
  // 0 1 1
  // 0 0 0
  CellType.NorthEastPipe -> List((1, 0), (1, 1), (2, 1)),
  // 0 1 0
  // 1 1 0
  // 0 0 0
  CellType.NorthWestPipe -> List((1, 0), (1, 1), (0, 1)),
  // 0 0 0
  // 1 1 0
  // 0 1 0
  CellType.SouthWestPipe -> List((1, 1), (1, 2), (0, 1)),
  // 0 0 0
  // 0 1 1
  // 0 1 0
  CellType.SouthEastPipe -> List((1, 1), (1, 2), (2, 1)),
  // 0 0 0
  // 0 0 0
  // 0 0 0
  CellType.Ground -> Nil,
  // 0 1 0
  // 1 1 1
  // 0 1 0
  CellType.Start -> List((1, 0), (1, 1), (1, 2), (0, 1), (2, 1))
)

type WallLocation = (Int, Int)

val walls: Set[WallLocation] = cellInLoop.flatMap { cellLocation =>
  val cellType = cellTypes(cellLocation)
  cellToWall(cellType).map { case (dx, dy) =>
    (cellLocation._1 * 3 + dx, cellLocation._2 * 3 + dy)
  }
}

val maxWallX = cellTypes.keys.map(_._1).max * 3
val maxWallY = cellTypes.keys.map(_._2).max * 3

def isInWallGrid(wl: WallLocation): Boolean = {
  wl._1 >= 0 && wl._1 <= maxWallX && wl._2 >= 0 && wl._2 <= maxWallY
}

val outdoor: Set[WallLocation] = {
  // flooding algorithm
  var toVisit = (0 to maxWallX).map((_, 0)).toList
  var visited = Set.empty[WallLocation]
  var result = Set.empty[WallLocation]
  while (toVisit.nonEmpty) {
    val current = toVisit.head
    toVisit = toVisit.tail
    if (!visited.contains(current)) {
      visited += current
      if (!walls.contains(current)) {
        result += current
        val neighbours = List((current._1 + 1, current._2), (current._1, current._2 + 1), (current._1 - 1, current._2), (current._1, current._2 - 1))
        val newNeighbours = neighbours.filter(isInWallGrid).filterNot(visited.contains)
        toVisit ++= newNeighbours
      }
    }
  }

  result
}

for {
  y <- 0 to maxWallY
  x <- 0 to maxWallX
} {
  val cell = (x, y)
  if (x == 0 && y > 0) println()
  if (walls.contains(cell)) print("#")
  else if (outdoor.contains(cell)) print(".")
  else print(" ")
}

def isIndoor(cell: CellLocation): Boolean = {
  val wx = cell._1 * 3
  val wy = cell._2 * 3
  !(cellInLoop.contains(cell) || outdoor.contains((wx, wy)))
}

val indoorCells = cellTypes.filter(kvp => isIndoor(kvp._1)).keys.toSet

// for {
//   y <- 0 to cellTypes.keys.map(_._2).max
//   x <- 0 to cellTypes.keys.map(_._1).max
//   c = (x, y)
// } {
//   if (x == 0 && y > 0) println()
//   if (cellInLoop.contains(c)) print(cellTypeToChar(cellTypes(c)))
//   else if (indoorCells.contains(c)) print("I")
//   else print(".")
// }

val part2 = indoorCells.size