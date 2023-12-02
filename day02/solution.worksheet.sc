import $dep.`org.scala-lang.modules:scala-parser-combinators_3:2.3.0`
import scala.util.parsing.combinator._

enum Color:
  case Red, Green, Blue

val colors = Color.values.toList

case class Turn(shown: Map[Color, Int])
case class Game(id: Int, turns: List[Turn])

object Parser extends RegexParsers with PackratParsers {
    val number: PackratParser[Int] = """-?\d+""".r ^^ { _.toInt }
    val color: PackratParser[Color] = "red" ^^ { _ => Color.Red } | "green" ^^ { _ => Color.Green } | "blue" ^^ { _ => Color.Blue }
    val colorNumber: PackratParser[(Color, Int)] = number ~ color ^^ { case n ~ c => c -> n }
    val colorNumberList: PackratParser[List[(Color, Int)]] = repsep(colorNumber, ",")
    val turn: PackratParser[Turn] = colorNumberList ^^ { l => Turn(l.toMap) }
    val turns: PackratParser[List[Turn]] = repsep(turn, ";")
    val gameId: PackratParser[Game] = ("Game" ~> number <~ ":") ~ turns ^^ { case id ~ t => Game(id, t) }
    val all = repsep(gameId, "")
}

val rawInput = scala.io.Source.fromFile("./day02/input.txt").mkString
val maybeParsedInput = Parser.parseAll(Parser.all, rawInput)
val parsedInput = maybeParsedInput.get

val maxForColor = Map(Color.Red -> 12, Color.Green -> 13, Color.Blue -> 14)

def isTurnPossible(turn: Turn): Boolean = {
    colors.forall(c => turn.shown.getOrElse(c, 0) <= maxForColor(c))
}

def isGamePossible(game: Game): Boolean = {
    game.turns.forall(isTurnPossible)
}

val possibleGames = parsedInput.filter(isGamePossible)
val part1Result = possibleGames.map(_.id).sum

def minForTurn(turn: Turn): Map[Color, Int] = {
  colors.map(c => c -> turn.shown.getOrElse(c, 0)).toMap
}

def minForGame(game: Game): Map[Color, Int] = {
  val minForTurns = game.turns.map(minForTurn)
  colors.map(c => c -> minForTurns.map(_(c)).max).toMap
}

def scoreForGame(game: Game): Int = {
  val minForGameMap = minForGame(game)
  
  colors.map(c => minForGameMap(c)).product
}

val part2Result = parsedInput.map(scoreForGame).sum