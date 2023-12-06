import $dep.`org.scala-lang.modules:scala-parser-combinators_3:2.3.0`
import scala.util.parsing.combinator._

// Time:      7  15   30
// Distance:  9  40  200

case class Race(
  time: Long,
  distance: Long
)

object Parser extends RegexParsers with PackratParsers {
  val times: PackratParser[List[Long]] = "Time:" ~> rep("\\d+".r) ^^ { _.map(_.toLong) }
  val distances: PackratParser[List[Long]] = "Distance:" ~> rep("\\d+".r) ^^ { _.map(_.toLong) }
  val input: PackratParser[List[Race]] = times ~ distances ^^ {
    case times ~ distances => times.zip(distances).map(td => Race(td._1, td._2) )
  }
}

val rawInput = scala.io.Source.fromFile("./day06/input.txt").mkString
val maybeParsedInput = Parser.parseAll(Parser.input, rawInput)
val parsedInput = maybeParsedInput.get

case class RaceResult(
  holdedFor: Long,
  distanceTotal: Long,
  race: Race
)

def allHoldings(race: Race): Iterator[RaceResult] = {
  for {
    holdedFor <- (0L to race.time).iterator
  } yield {
    val timeRemain = race.time - holdedFor
    val speed = holdedFor
    RaceResult(
      holdedFor = holdedFor,
      distanceTotal = speed * timeRemain,
      race = race
    )
  }
}

def isWinning(result: RaceResult): Boolean = {
  result.distanceTotal > result.race.distance
}

def countWinning(race: Race): Long = {
  allHoldings(race).filter(isWinning).size
}

val part1 = parsedInput.map(countWinning).product

val part2Time = parsedInput.map(_.time.toString()).mkString("").toLong
val part2Distance = parsedInput.map(_.distance.toString()).mkString("").toLong
val part2Race = Race(part2Time, part2Distance)

val part2 = countWinning(part2Race)