import scala.annotation.tailrec
import $dep.`org.scala-lang.modules:scala-parser-combinators_3:2.3.0`
import scala.util.parsing.combinator._

// RLRLRLLLRRRLLR
// AAA = (BBB, CCC)
// BBB = (DDD, EEE)

enum Instruction:
  case Left
  case Right

case class Connection(
  key: String,
  left: String,
  right: String
)

case class ParsedInput(
  instructions: List[Instruction],
  connections: List[Connection]
)

object Parser extends RegexParsers with PackratParsers {
  val instruction: PackratParser[Instruction] = ("L" | "R") ^^ {
    case "L" => Instruction.Left
    case "R" => Instruction.Right
  }

  val instructions: PackratParser[List[Instruction]] = rep(instruction)
  
  val key: PackratParser[String] = "[A-Z]+".r ^^ { _.toString }

  val connection: PackratParser[Connection] = key ~ "=" ~ "(" ~ key ~ "," ~ key ~ ")" ^^ {
    case key ~ "=" ~ "(" ~ left ~ "," ~ right ~ ")" => Connection(key, left, right)
  }

  val connections: PackratParser[List[Connection]] = rep(connection)


  val input: PackratParser[ParsedInput] = instructions ~ connections ^^ {
    case instructions ~ connections => ParsedInput(instructions, connections)
  }
}

val rawInput = scala.io.Source.fromFile("./day08/input.txt").mkString
val maybeParsedInput = Parser.parseAll(Parser.input, rawInput)
val parsedInput = maybeParsedInput.get

val connectionMap = parsedInput.connections.map(c => c.key -> c).toMap

def followInstruction(currentKey: String, instruction: Instruction): String = {
  val connection = connectionMap(currentKey)
  instruction match {
    case Instruction.Left => connection.left
    case Instruction.Right => connection.right
  }
}

def graphPath(startKey: String, instructions: List[Instruction]): LazyList[String] = {
  var current = startKey
  for {
    i <- LazyList.continually(instructions).flatten
  } yield {
    current = followInstruction(current, i)
    current
  }
}

def path = graphPath("AAA", parsedInput.instructions)

val part1 = path.view.takeWhile(_ != "ZZZ").count(_ => true) + 1

// part 2

type LocationId = Int
type InstructionId = Int

case class ConnectionById(
  key: Int,
  left: Int,
  right: Int
)

val locationToId = parsedInput.connections.map(_.key).zipWithIndex.map((k, i) => k -> i).toMap
val connectionsById = parsedInput.connections.map(c => ConnectionById(locationToId(c.key), locationToId(c.left), locationToId(c.right)))
val instructionsArray = parsedInput.instructions.toArray
val connectionsArray = connectionsById.toArray

case class LoopState(
  currentLocationId: LocationId,
  currentInstructionId: InstructionId,
)

val starts = parsedInput.connections.filter(_.key.endsWith("A")).toArray.map(s => locationToId(s.key))
val ends = parsedInput.connections.filter(_.key.endsWith("Z")).toArray.map(s => locationToId(s.key))

trait Looper[+TState] {
  def counter: Long
  def currentStep: TState
  def next: Looper[TState]
}

object Looper:
  def from[TState](initialState: TState)(stepper: TState => TState): Looper[TState] = {
    new Looper[TState] {
      var counter = 0L
      var currentStep = initialState
      def next = {
        counter += 1
        currentStep = stepper(currentStep)
        this
      }
    }
  }

  def filter[TState](looper: Looper[TState])(predicate: TState => Boolean): Looper[TState] = {
    class FilteredLooper[TState](currentLooper: Looper[TState], predicate: TState => Boolean) extends Looper[TState] {
      def counter = currentLooper.counter
      def currentStep = currentLooper.currentStep
      def next = {
        var nextLooper = currentLooper.next
        while (!predicate(nextLooper.currentStep)) {
          nextLooper = nextLooper.next
        }
        FilteredLooper(nextLooper, predicate)
      }
    }

    val startingLooper = new FilteredLooper(looper, predicate)

    if (predicate(startingLooper.currentStep)) {
      startingLooper
    } else {
      startingLooper.next
    }
  }

  def skipper[TState](looper: Looper[TState]): Looper[TState] = {
    val firstState = looper.currentStep
    val firstCount = looper.counter
    val next = looper.next
    val nextState = next.currentStep
    val nextCount = next.counter
    if (firstState != nextState) {
      var c = looper
      for {
        i <- 0 to 10
      } {
        print(s"$i: ")
        println(c.currentStep)
        c = c.next
      }
      
      throw new NotImplementedError("Can't use skipper on a looper that doesn't repeat after the first step")
    }
    val delta = nextCount - firstCount
    
    class SkipperLooper[TState](val counter: Long, val currentStep: TState, delta: Long) extends Looper[TState] {
      def next = SkipperLooper(counter + delta, currentStep, delta)
    }
    SkipperLooper(firstCount, firstState, delta)
  }

val loopers = starts.map(start => {
  val initialState = LoopState(start, 0)
  val looper = Looper.from(initialState)(state => {
    val currentInstruction = instructionsArray(state.currentInstructionId)
    val currentConnection = connectionsArray(state.currentLocationId)
    val nextLocationId = currentInstruction match {
      case Instruction.Left => currentConnection.left
      case Instruction.Right => currentConnection.right
    }
    val nextInstructionId = (state.currentInstructionId + 1) % instructionsArray.length
    LoopState(nextLocationId, nextInstructionId)
  })
  val filtered = Looper.filter(looper)(state => ends.contains(state.currentLocationId))
  val skipper = Looper.skipper(filtered)
  skipper
})

def mergeTwoLooper(looper1: Looper[_], looper2: Looper[_]): Looper[Unit] = {
  class MergerLooper(looper1: Looper[Any], looper2: Looper[Any]) extends Looper[Unit] {
    def counter = looper1.counter
    def currentStep = ()
    def next = {
      var nextLooper1 = looper1
      var nextLooper2 = looper2

      // no do while =(
      if (nextLooper1.counter < nextLooper2.counter) {
          nextLooper1 = nextLooper1.next
        } else {
          nextLooper2 = nextLooper2.next
      }

      while (nextLooper1.counter != nextLooper2.counter) {
        if (nextLooper1.counter < nextLooper2.counter) {
          nextLooper1 = nextLooper1.next
        } else {
          nextLooper2 = nextLooper2.next
        }
      }

      MergerLooper(nextLooper1, nextLooper2)
    }
  }

  val merged = MergerLooper(looper1, looper2)
  if (looper1.currentStep != looper2.currentStep) {
    merged.next
  } else {
    merged
  }
}

def mergeLoopers(loopers: List[Looper[_]]): Looper[Unit] = {
  loopers match {
    case Nil => throw new IllegalArgumentException("Can't merge empty list of loopers")
    case looper :: Nil => throw new IllegalArgumentException("Can't merge list of one looper")
    case looper1 :: looper2 :: Nil => Looper.skipper(mergeTwoLooper(looper1, looper2))
    case looper1 :: looper2 :: rest => Looper.skipper(mergeTwoLooper(looper1, mergeLoopers(looper2 :: rest)))
  }
}

val mergedLooper = mergeLoopers(loopers.toList)

val part2 = mergedLooper.counter