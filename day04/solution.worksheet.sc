
val rawInput = scala.io.Source.fromFile("./day04/input.txt").mkString

case class Card(
  winnings: List[Int],
  deck: List[Int],
)

def parseInput(input: String): List[Card] =
  val lines = input.split(sys.props("line.separator")).toList
  for {
    l <- lines.filter(_.nonEmpty)
  } yield {
    val sep1 = l.indexOf(':')
    val sep2 = l.indexOf('|')
    if sep1 == -1 || sep2 == -1 then
      throw new Exception(s"Invalid input: $l")
    val winnings = l.substring(sep1 + 1, sep2).split(' ').filter(_.nonEmpty).map(_.toInt).toList
    val deck = l.substring(sep2 + 1).split(' ').filter(_.nonEmpty).map(_.toInt).toList
    Card(winnings, deck)
  }

val input = parseInput(rawInput)

def winningCard(card: Card): List[Int] =
  card.winnings.filter(card.deck.contains)

def scoreCard(card: Card): Int =
  val matchNumber = winningCard(card).length
  Math.pow(2, matchNumber - 1).toInt

def scoreGame(cards: List[Card]): Int =
  cards.map(scoreCard).sum

val part1 = scoreGame(input)

var copiesCount = (for {
  i <- 0 until input.length
} yield (i -> 1L)).toMap

for {
  i <- 0 until input.length
} {
  val card = input(i)
  val score = winningCard(card).length
  val multiplier = copiesCount(i)
  println(s"$i: $score * count ($multiplier)")

  for {
    j <- i+1 to i+score
    if j < input.length
  } {
    copiesCount = copiesCount + (j -> (copiesCount(j) + multiplier))
  }
}

for {
  i <- 0 until input.length
} {
  println(s"$i: ${copiesCount(i)}")
}

val part2 = copiesCount.values.sum