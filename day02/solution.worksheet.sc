import $dep.`org.scala-lang.modules:scala-parser-combinators_3:2.3.0`
import scala.util.parsing.combinator._

enum Color:
  case Red, Green, Blue

case class Turn(shown: Map[Color, Int])
case class Game(id: Int, turns: List[Turn])

object Parser extends RegexParsers with PackratParsers {
    val number: PackratParser[Int] = """-?\d+""".r ^^ { _.toInt }
    val color: PackratParser[Color] = "red" ^^ { _ => Color.Red } | "green" ^^ { _ => Color.Green } | "blue" ^^ { _ => Color.Blue }
    val colorNumber: PackratParser[(Color, Int)] = number ~ color ^^ { case n ~ c => (c, n) }
    val colorNumberList: PackratParser[List[(Color, Int)]] = repsep(colorNumber, ",")
    val turn: PackratParser[Turn] = colorNumberList ^^ { l => Turn(l.toMap) }
    val turns: PackratParser[List[Turn]] = repsep(turn, ";")
    val gameId: PackratParser[Game] = ("Game" ~> number <~ ":") ~ turns ^^ { case id ~ t => Game(id, t) }
    val all = repsep(gameId, "")
}

val rawInput = scala.io.Source.fromFile("./day02/input.txt").mkString
val maybeParsedInput = Parser.parseAll(Parser.all, rawInput)
val parsedInput = maybeParsedInput.get

val maxRed = 12
val maxGreen = 13
val maxBlue = 14

def isTurnPossible(turn: Turn): Boolean = {
    val red = turn.shown.getOrElse(Color.Red, 0)
    val green = turn.shown.getOrElse(Color.Green, 0)
    val blue = turn.shown.getOrElse(Color.Blue, 0)
    red <= maxRed && green <= maxGreen && blue <= maxBlue
}

def isGamePossible(game: Game): Boolean = {
    game.turns.forall(isTurnPossible)
}

val possibleGames = parsedInput.filter(isGamePossible)
val part1Result = possibleGames.map(_.id).sum

def minForTurn(turn: Turn): Map[Color, Int] = {
  val red = turn.shown.getOrElse(Color.Red, 0)
  val green = turn.shown.getOrElse(Color.Green, 0)
  val blue = turn.shown.getOrElse(Color.Blue, 0)
  Map(Color.Red -> red, Color.Green -> green, Color.Blue -> blue)
}

def minForGame(game: Game): Map[Color, Int] = {
  val minForTurns = game.turns.map(minForTurn)
  val minRed = minForTurns.map(_(Color.Red)).max
  val minGreen = minForTurns.map(_(Color.Green)).max
  val minBlue = minForTurns.map(_(Color.Blue)).max

  Map(Color.Red -> minRed, Color.Green -> minGreen, Color.Blue -> minBlue)
}

def scoreForGame(game: Game): Int = {
  val minForGameMap = minForGame(game)
  
  minForGameMap(Color.Red) * minForGameMap(Color.Green) * minForGameMap(Color.Blue)
}

val part2Result = parsedInput.map(scoreForGame).sum