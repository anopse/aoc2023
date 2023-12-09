val rawInput = scala.io.Source.fromFile("./day09/input.txt").mkString
val lines = rawInput.split(sys.props("line.separator")).toList
val sequences: List[Sequence] = lines.map(l =>l.split(" ").map(_.toInt).toList)

type Sequence = List[Int]

def isLinear(sequence: Sequence): Boolean = {
  if (sequence.size < 2) true
  else {
    val delta = sequence(1) - sequence(0)
    sequence.indices.forall(i => sequence(i) == sequence(0) + i * delta)
  }
}

def solveSequence(sequence: Sequence): Int = {
  if (isLinear(sequence)) {
    val delta = sequence(1) - sequence(0)
    sequence.last + delta
  } else {
    val deltaSequence = sequence.indices.drop(1).map(i => sequence(i) - sequence(i-1)).toList
    val nextDelta = solveSequence(deltaSequence)
    sequence.last + nextDelta
  }
}

val nextValues = sequences.map(solveSequence)
val part1 = nextValues.sum

def solveSequenceBackward(sequence: Sequence): Int = {
  if (isLinear(sequence)) {
    val delta = sequence(1) - sequence(0)
    sequence.head - delta
  } else {
    val deltaSequence = sequence.indices.drop(1).map(i => sequence(i) - sequence(i-1)).toList
    val nextDelta = solveSequenceBackward(deltaSequence)
    sequence.head - nextDelta
  }
}

val nextValuesBackward = sequences.map(solveSequenceBackward)
val part2 = nextValuesBackward.sum