import scala.annotation.tailrec

val rawInput = scala.io.Source.fromFile("./day15/input.txt").mkString

val words = rawInput.split(",").toList

def asciiValue(c: Char): Int = c.toInt

def scoreWord(word: String): Int = word.foldLeft(0)((acc, c) => ((acc + asciiValue(c)) * 17) % 256)

val part1 = words.map(scoreWord).sum

enum Op:
  case Remove(label: String)
  case Insert(label: String, power: Int)

def parseOp(op: String): Op = {
  if (op.endsWith("-")) {
    Op.Remove(op.dropRight(1))
  } else {
    val parts = op.split("=")
    Op.Insert(parts(0), parts(1).toInt)
  }
}

val ops = words.map(parseOp)

def boxOf(label: String): Int = scoreWord(label)

case class BoxLens(label: String, power: Int)
case class BoxState(lens: List[BoxLens])

type State = Map[Int, BoxState]

val emptyBoxes: State = Map.empty[Int, BoxState].withDefault(_ => BoxState(List.empty))

def applyOp(op: Op, state: State): State = {
  op match {
    case Op.Remove(label) =>
      val box = boxOf(label)
      val boxState = state(box)
      val newBoxState = boxState.copy(lens = boxState.lens.filter(_.label != label))
      state + (box -> newBoxState)
    case Op.Insert(label, power) =>
      val box = boxOf(label)
      val boxState = state(box)
      val findIndex = boxState.lens.indexWhere(_.label == label)
      if (findIndex == -1) {
        val newBoxState = boxState.copy(lens = boxState.lens :+ BoxLens(label, power))
        state + (box -> newBoxState)
      } else {
        val newBoxState = boxState.copy(lens = boxState.lens.updated(findIndex, BoxLens(label, power)))
        state + (box -> newBoxState)
      }
  }
}

def applyOps(ops: List[Op], initialState: State): State = {
  ops.foldLeft(initialState)((acc, op) => 
    println(acc)
    applyOp(op, acc)
    )
}

val afterOps = applyOps(ops, emptyBoxes)

def scoreBox(box: Int, state: State): Int = {
  val boxState = state(box)

  val scores = for {
    i <- boxState.lens.indices.toList
  } yield {
    val lens = boxState.lens(i)
    val power = lens.power
    (box + 1) * power * (i + 1)
  }

  scores.sum
}

def scoreBoxes(state: State): Long = {
  state.keys.toList.map(box => scoreBox(box, state).toLong).sum
}

val part2 = scoreBoxes(afterOps)