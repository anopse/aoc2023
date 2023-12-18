import scala.annotation.tailrec
import $dep.`org.scala-lang.modules:scala-parser-combinators_3:2.3.0`
import scala.util.parsing.combinator._

final case class Coord(x: Long, y: Long) {
  def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
  def *(times: Int): Coord = Coord(x * times, y * times)
}

sealed trait Stepper:
  def asDelta: Coord
  def apply(coord: Coord): Coord = coord + asDelta
  def inverse(): Stepper
  def rotateLeft(): Stepper
  def rotateRight(): Stepper

val Up: Stepper = new Stepper:
  val asDelta: Coord = Coord(0, -1)
  def inverse(): Stepper = Down
  def rotateLeft(): Stepper = Left
  def rotateRight(): Stepper = Right
  override def toString(): String = "Up"

val Down: Stepper = new Stepper:
  val asDelta: Coord = Coord(0, 1)
  def inverse(): Stepper = Up
  def rotateLeft(): Stepper = Right
  def rotateRight(): Stepper = Left
  override def toString(): String = "Down"

val Left: Stepper = new Stepper:
  val asDelta: Coord = Coord(-1, 0)
  def inverse(): Stepper = Right
  def rotateLeft(): Stepper = Down
  def rotateRight(): Stepper = Up
  override def toString(): String = "Left"

val Right: Stepper = new Stepper:
  val asDelta: Coord = Coord(1, 0)
  def inverse(): Stepper = Left
  def rotateLeft(): Stepper = Up
  def rotateRight(): Stepper = Down
  override def toString(): String = "Right"

val Directions = List(Up, Down, Left, Right)

final case class Instruction(direction: Stepper, steps: Int, color: String)

// Example input :
// R 6 (#70c710)
// D 5 (#0dc571)

object Parser extends RegexParsers with PackratParsers {
  val directionParser: PackratParser[Stepper] = ("R" | "D" | "L" | "U") ^^ {
    case "R" => Right
    case "D" => Down
    case "L" => Left
    case "U" => Up
  }

  val numberParser: PackratParser[Int] = "[0-9]+".r ^^ { _.toInt }

  val hexColorParser: PackratParser[String] = ("#" ~> "[0-9a-f]{6}".r) ^^ {
    case hex => hex
  }

  val instruction: PackratParser[Instruction] = directionParser ~ numberParser ~ ("(" ~> hexColorParser <~ ")") ^^ {
    case direction ~ steps ~ color => Instruction(direction, steps, color)
  }

  val input: PackratParser[List[Instruction]] = rep(instruction)
}

val rawInput = scala.io.Source.fromFile("./day18/input.txt").mkString
val maybeParsedInput = Parser.parseAll(Parser.input, rawInput)
val parsedInput = maybeParsedInput.get

val initialCoord = Coord(0, 0)

final def applyInstruction[S](stateFunction: (S, Coord, Coord, Instruction) => S)(coord: Coord, instruction: Instruction, state: S): (S, Coord) = {
  var newCoord = coord + (instruction.direction.asDelta * instruction.steps)
  var newState = stateFunction(state, coord, newCoord, instruction)
  newState -> newCoord
}

final def applyInstructions[S](stateFunction: (S, Coord, Coord, Instruction) => S)(instructions: List[Instruction], state: S): (S, Coord) = {
  instructions.foldLeft((state, initialCoord)) {
    case ((state, coord), instruction) => applyInstruction(stateFunction)(coord, instruction, state)
  }
}

case class Line(start: Coord, end: Coord)

type DrawingSet = List[Line]

final def draw(state: List[Line], beginCoord: Coord, endCoord: Coord, instruction: Instruction): List[Line] = {
  val line = Line(beginCoord, endCoord)
  line::state
}

def drawingFromInstructions(instructions: List[Instruction]): DrawingSet = {
  val (drawing, _) = applyInstructions(draw)(instructions, Nil)
  drawing.reverse
}

val drawing = drawingFromInstructions(parsedInput)

final def computeWallArea(walls: DrawingSet): Long = {
  walls.foldLeft(0L) {
    case (acc, line) => {
      val dx = line.end.x - line.start.x
      val dy = line.end.y - line.start.y
      acc + Math.abs(dx + dy)
    }
  }
}

final def computeInsideArea(walls: DrawingSet): Long = {
  // shoelace formula
  walls.map { line =>
    val x = (line.start.x * line.end.y) - (line.start.y * line.end.x)
    x
  }.sum / 2
}

final def perimeter(instructions: List[Instruction]): Long = {
  instructions.map(_.steps).sum
}

final def computeArea(walls: DrawingSet, instructions: List[Instruction]): Long = {
  val inside = computeInsideArea(walls)
  val p = perimeter(instructions)
  inside + (p / 2) + 1
}

val part1 = computeArea(drawing, parsedInput)

val part2Instructions = parsedInput.map { instruction =>
  val hex = instruction.color.take(5)
  val direction = instruction.color.drop(5) match {
    case "0" => Right
    case "1" => Down
    case "2" => Left
    case "3" => Up
  }
  val steps = Integer.parseInt(hex, 16)
  Instruction(direction, steps, instruction.color)
}

final def part2Drawing = drawingFromInstructions(part2Instructions)

val part2 = computeArea(part2Drawing, part2Instructions)