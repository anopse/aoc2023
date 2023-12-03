
val rawInput = scala.io.Source.fromFile("./day03/input.txt").mkString

enum Token:
  case Digit(i: Int)
  case Symbol(s: Char)
  case Dot

case class Coord(x: Int, y: Int)

def parseInput(input: String): Map[Coord, Token] =
  val lines = input.split("\n")
  var map = Map.empty[Coord, Token]
  for {
    y <- lines.indices
  } {
    val line = lines(y).filterNot(_.isWhitespace)
    for {
      x <- line.indices
      c = line(x)
    } {
      val coord = Coord(x, y)
      val token = c match {
        case x if x.isDigit => Token.Digit(x.asDigit)
        case '.' => Token.Dot
        case s => Token.Symbol(s)
      }
      map = map + (coord -> token)
    }
  }
  map

val inputMap = parseInput(rawInput)

case class Number(value: String, begin: Coord)

case class Group(numbers: List[Number], symbol: Token.Symbol, coord: Coord)

def neightbours(coord: Coord): List[Coord] =
  val coords = for {
    x <- coord.x - 1 to coord.x + 1
    y <- coord.y - 1 to coord.y + 1
    if !(x == coord.x && y == coord.y)
  } yield Coord(x, y)
  coords.toList

def horizontalNeightbours(coord: Coord): List[Coord] =
  List(
    Coord(coord.x - 1, coord.y),
    Coord(coord.x + 1, coord.y),
  )

def flattenGroup(group: Group): Group =
  // merge adjacent numbers
  var resultingGroup = group
  val numbers = group.numbers.sortBy(-_.begin.x)
  for {
    current <- numbers
  } {
    val currentCoord = current.begin
    val nextCoord = current.begin.copy(x = currentCoord.x + 1)
    val next = resultingGroup.numbers.find(_.begin == nextCoord)
    next match {
      case Some(n) =>
        val newValue = (current.value.toString() + n.value.toString())
        val newNumber = Number(newValue, current.begin)
        val newNumbers = resultingGroup.numbers.filter(c => !(c.begin == currentCoord || c.begin == nextCoord)) :+ newNumber
        resultingGroup = resultingGroup.copy(numbers = newNumbers)
      case None => ()
    }
  }
  resultingGroup

def makeGroup(inputMap: Map[Coord, Token], coord: Coord, symbol: Token.Symbol): Group =
  var numbers = List.empty[Number]
  var visited = Set.empty[Coord]
  var toVisit = neightbours(coord)
  while toVisit.nonEmpty do
    val current = toVisit.head
    toVisit = toVisit.tail
    if !visited.contains(current) then
      visited = visited + current
      inputMap.get(current) match
        case Some(Token.Digit(i)) =>
          numbers = numbers :+ Number(i.toString, current)
          toVisit = toVisit ++ horizontalNeightbours(current)
        case _ => ()
  flattenGroup(Group(numbers, symbol, coord))
  

def findGroups(inputMap: Map[Coord, Token]): List[Group] =
  var groups = List.empty[Group]
  for {
    (coord, maybeSymbol) <- inputMap
    symbol <- maybeSymbol match {
      case s: Token.Symbol => Some(s)
      case _ => None
    }
  } {
    val g = makeGroup(inputMap, coord, symbol)
    groups = groups :+ g
  }
  groups

val groups = findGroups(inputMap)
val validNumbers = groups.flatMap(_.numbers).toSet.toList.sortBy(e => e.begin.y * 10000 + e.begin.x)

for {
  n <- validNumbers.map(_.value)
} println(n)

val part1 =validNumbers.map(_.value.toInt).sum


// part 2

val validGears = groups.filter(g => g.symbol.s == '*' && g.numbers.size == 2)
val part2 = validGears.map(g => g.numbers.map(_.value.toInt).product).sum

