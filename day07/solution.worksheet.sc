val rawInput = scala.io.Source.fromFile("./day07/input.txt").mkString

case class Rank(
  value: Int,
  part2Value: Int,
  name: String,
  symbol: Char
)

val allRanks = List(
  Rank(2, 2, "Two", '2'),
  Rank(3, 3, "Three", '3'),
  Rank(4, 4, "Four", '4'),
  Rank(5, 5, "Five", '5'),
  Rank(6, 6, "Six", '6'),
  Rank(7, 7, "Seven", '7'),
  Rank(8, 8, "Eight", '8'),
  Rank(9, 9, "Nine", '9'),
  Rank(10, 10, "Ten", 'T'),
  Rank(11, 1, "Jack|Joker", 'J'),
  Rank(12, 12, "Queen", 'Q'),
  Rank(13, 13, "King", 'K'),
  Rank(14, 14, "Ace", 'A')
)

val joker = allRanks.find(_.symbol == 'J').get

val charToRank = allRanks.map(r => r.symbol -> r).toMap

case class Hand(
  cards: List[Rank],
  bet: Int
)

def parseLine(line: String): Hand = {
  val parts = line.split(" ")
  val cards = parts(0).map(c => charToRank(c)).toList
  val bet = parts(1).toInt
  Hand(cards, bet)
}

val hands = rawInput.split(sys.props("line.separator")).map(parseLine).toList

case class HandKind(
  name: String,
  handScore: Int,
  matcher: Hand => Option[HandResult],
  part2Matcher: Hand => Option[HandResult]
)

case class HandResult(
  kind: HandKind,
  hand: Hand
)

val FiveOfAKind: HandKind = HandKind(
  "Five of a Kind",
  handScore = 7,
  hand => {
    val cards = hand.cards
    val rank = cards.head
    val isFiveOfAKind = cards.forall(_ == rank)
    if isFiveOfAKind then
      Some(HandResult(FiveOfAKind, hand))
    else
      None
  },
  hand => {
    val cards = hand.cards
    val notJokers = cards.filter(_ != joker)
    val rank = if notJokers.isEmpty then joker else notJokers.head
    val isFiveOfAKind = cards.forall(c => c == rank || c == joker)
    if isFiveOfAKind then
      Some(HandResult(FiveOfAKind, hand))
    else
      None
  }
)

val FourOfAKind: HandKind = HandKind(
  "Four of a Kind",
  handScore = 6,
  hand => {
    val cards = hand.cards
    val ranks = cards.groupBy(identity).map((k, v) => (k, v.length))
    val rank = ranks.find((k, v) => v == 4).map((k, v) => k)
    rank match {
      case Some(r) => Some(HandResult(FourOfAKind, hand))
      case None => None
    }
  },
  hand => {
    val cards = hand.cards
    val notJokers = cards.filter(_ != joker)
    val jokerCounts = cards.count(_ == joker)
    val ranks = notJokers.groupBy(identity).map((k, v) => (k, v.length)).toList.sortBy(-_._1.value)
    
    val rank = ranks.find((k, v) => v + jokerCounts == 4).map((k, v) => k)
    rank match {
      case Some(r) => Some(HandResult(FourOfAKind, hand))
      case None => None
    }
  }
)

val FullHouse: HandKind = HandKind(
  "Full House",
  handScore = 5,
  hand => {
    val cards = hand.cards
    val ranks = cards.groupBy(identity).map((k, v) => (k, v.length))
    val rank1 = ranks.find((k, v) => v == 3).map((k, v) => k)
    val rank2 = ranks.find((k, v) => v == 2).map((k, v) => k)

    (rank1, rank2) match {
      case (Some(r1), Some(r2)) => Some(HandResult(FullHouse, hand))
      case _ => None
    }
  },
  hand => {
    val cards = hand.cards
    val notJokers = cards.filter(_ != joker)
    val jokerCounts = cards.count(_ == joker)
    val ranks = notJokers.groupBy(identity).map((k, v) => (k, v.length)).toList.sortBy(-_._1.value)
    val rank1 = ranks.find((k, v) => v == 3 - jokerCounts).map((k, v) => k)
    val rank2 = ranks.find((k, v) => v == 2 && Some(k) != rank1).map((k, v) => k)
    val rank1Count = cards.count(Some(_) == rank1)
    val rank2Count = cards.count(Some(_) == rank2)

    (rank1, rank2) match {
      case (Some(r1), Some(r2)) if rank1Count + rank2Count + jokerCounts == 5 => Some(HandResult(FullHouse, hand))
      case _ => None
    }
  }
)

val ThreeOfAKind: HandKind = HandKind(
  "Three of a Kind",
  handScore = 4,
  hand => {
    val cards = hand.cards
    val ranks = cards.groupBy(identity).map((k, v) => (k, v.length))
    val rank = ranks.find((k, v) => v == 3).map((k, v) => k)
    rank match {
      case Some(r) => Some(HandResult(ThreeOfAKind, hand))
      case None => None
    }
  },
  hand => {
    val cards = hand.cards
    val notJokers = cards.filter(_ != joker)
    val jokerCounts = cards.count(_ == joker)
    val ranks = notJokers.groupBy(identity).map((k, v) => (k, v.length)).toList.sortBy(-_._1.value)
    val rank = ranks.find((k, v) => v + jokerCounts == 3).map((k, v) => k)
    rank match {
      case Some(r) => Some(HandResult(ThreeOfAKind, hand))
      case None => None
    }
  }
)

val TwoPairs: HandKind = HandKind(
  "Two Pairs",
  handScore = 3,
  hand => {
    val cards = hand.cards
    val ranks = cards.groupBy(identity).map((k, v) => (k, v.length))
    val rank1 = ranks.find((k, v) => v == 2).map((k, v) => k)
    val rank2 = ranks.find((k, v) => v == 2 && Some(k) != rank1).map((k, v) => k)

    (rank1, rank2) match {
      case (Some(r1), Some(r2)) => Some(HandResult(TwoPairs, hand))
      case _ => None
    }
  },
  hand => {
    // cannot get a valuable two pairs using J
    val cards = hand.cards
    val ranks = cards.groupBy(identity).map((k, v) => (k, v.length))
    val rank1 = ranks.find((k, v) => v == 2).map((k, v) => k)
    val rank2 = ranks.find((k, v) => v == 2 && Some(k) != rank1).map((k, v) => k)

    (rank1, rank2) match {
      case (Some(r1), Some(r2)) => Some(HandResult(TwoPairs, hand))
      case _ => None
    }
  }
)

val OnePair: HandKind = HandKind(
  "One Pair",
  handScore = 2,
  hand => {
    val cards = hand.cards
    val ranks = cards.groupBy(identity).map((k, v) => (k, v.length))
    val rank = ranks.find((k, v) => v == 2).map((k, v) => k)
    rank match {
      case Some(r) => Some(HandResult(OnePair, hand))
      case None => None
    }
  },
  hand => {
    val cards = hand.cards
    val notJokers = cards.filter(_ != joker)
    val jokerCounts = cards.count(_ == joker)
    val ranks = notJokers.groupBy(identity).map((k, v) => (k, v.length)).toList.sortBy(-_._1.value)
    val rank = ranks.find((k, v) => v + jokerCounts == 2).map((k, v) => k)
    rank match {
      case Some(r) => Some(HandResult(OnePair, hand))
      case None => None
    }
  }
)

val HighCard: HandKind = HandKind(
  "High Card",
  handScore = 1,
  hand => {
    val cards = hand.cards
    val rank = cards.maxBy(_.value)
    Some(HandResult(HighCard, hand))
  },
  hand => {
    val cards = hand.cards
    val rank = cards.maxBy(_.value)
    Some(HandResult(HighCard, hand))
  }
)

val allHandsKind = List(
  FiveOfAKind,
  FourOfAKind,
  FullHouse,
  ThreeOfAKind,
  TwoPairs,
  OnePair,
  HighCard
)

def computeHandResult(hand: Hand): HandResult = {
  val handResult = allHandsKind.map(_.matcher(hand)).find(_.isDefined).flatten
  handResult match {
    case Some(hr) => hr
    case None => throw new Exception(s"Invalid hand: $hand")
  }
}

def scoreHand(hand: Hand): Long = {
  val handResult = computeHandResult(hand)
  val len = hand.cards.length
  val handSubScore = hand.cards.zipWithIndex.map((r, i) => r.value * Math.pow(16, len-i).toLong).sum
  handResult.kind.handScore * 100000000000000L + handSubScore
}

val handsScores = hands.map(h => h -> scoreHand(h))

// from lowest to highest
val sortedHands = handsScores.sortBy(_._2).map(_._1)
val sortedBets = sortedHands.map(_.bet)

// for {
//   i <- 0 until sortedHands.length
//   hand = sortedHands(i)
//   handString = hand.cards.map(_.symbol).mkString
//   bet = hand.bet
//   result = computeHandResult(hand)
//   score = scoreHand(hand)
//   handKind = result.kind.name
// } println(s"$handString $bet    $score $handKind")

val part1 = sortedBets.view.zipWithIndex.map((b, i) => b * (i + 1L)).sum

// part2

def computeHandResultPart2(hand: Hand): HandResult = {
  val handResult = allHandsKind.map(_.part2Matcher(hand)).find(_.isDefined).flatten
  handResult match {
    case Some(hr) => hr
    case None => throw new Exception(s"Invalid hand: $hand")
  }
}

def scoreHandPart2(hand: Hand): Long = {
  val handResult = computeHandResultPart2(hand)
  val len = hand.cards.length
  val handSubScore = hand.cards.zipWithIndex.map((r, i) => r.part2Value * Math.pow(16, len-i).toLong).sum
  handResult.kind.handScore * 100000000000000L + handSubScore
}


val handsScoresPart2 = hands.map(h => h -> scoreHandPart2(h))

// from lowest to highest
val sortedHandsPart2 = handsScoresPart2.sortBy(_._2).map(_._1)
val sortedBetsPart2 = sortedHandsPart2.map(_.bet)

for {
  i <- 0 until sortedHandsPart2.length
  hand = sortedHandsPart2(i)
  handString = hand.cards.map(_.symbol).mkString
  bet = hand.bet
  result = computeHandResultPart2(hand)
  score = scoreHandPart2(hand)
  handKind = result.kind.name
} println(s"$handString $bet\t$score $handKind")

val part2 = sortedBetsPart2.view.zipWithIndex.map((b, i) => b * (i + 1L)).sum