import scala.annotation.tailrec
import $dep.`org.scala-lang.modules:scala-parser-combinators_3:2.3.0`
import scala.util.parsing.combinator._

// example input :
// px{a<2006:qkq,m>2090:A,rfg}
// pv{a>1716:R,A}
// lnx{m>1548:A,A}
// rfg{s<537:gd,x>2440:R,A}
//
// {x=787,m=2655,a=1222,s=2876}
// {x=1679,m=44,a=2067,s=496}
// {x=2036,m=264,a=79,s=2244}

enum XMAS:
  case X
  case M
  case A
  case S

val allXMAS = XMAS.values.toList

enum ComparOp:
  case LessThan
  case GreaterThan

opaque type WorkflowId = String

final case class Comparison(xmas: XMAS, comparOp: ComparOp, value: Int)

enum Goto:
  case Rejected
  case Accepted
  case GotoWorkflow(WorkflowId: WorkflowId)

final case class WorkflowInstruction(comparison: Comparison, goto: Goto)

final case class Workflow(id: WorkflowId, instructions: List[WorkflowInstruction], fallback: Goto)

type XMASValue = Map[XMAS, Int]

final case class ParsedInput(workflows: List[Workflow], xmasValues: List[XMASValue])

object Parser extends RegexParsers with PackratParsers {
  val xmas: PackratParser[XMAS] = ("x" | "m" | "a" | "s") ^^ {
    case "x" => XMAS.X
    case "m" => XMAS.M
    case "a" => XMAS.A
    case "s" => XMAS.S
  }

  val numberParser: PackratParser[Int] = "[0-9]+".r ^^ { _.toInt }

  val workflowId: PackratParser[WorkflowId] = "[a-z]{2,}".r ^^ { _.toString }

  val comparOp: PackratParser[ComparOp] = ("<" | ">") ^^ {
    case "<" => ComparOp.LessThan
    case ">" => ComparOp.GreaterThan
  }

  val comparison: PackratParser[Comparison] = xmas ~ comparOp ~ numberParser ^^ {
    case xmas ~ comparOp ~ value => Comparison(xmas, comparOp, value)
  }

  val goto: PackratParser[Goto] = ("A" | "R" | workflowId) ^^ {
    case "A" => Goto.Accepted
    case "R" => Goto.Rejected
    case workflowId => Goto.GotoWorkflow(workflowId)
  }

  val workflowInstruction: PackratParser[WorkflowInstruction] = (comparison <~ ":") ~ goto ^^ {
    case comparison ~ goto => WorkflowInstruction(comparison, goto)
  }

  val workflow: PackratParser[Workflow] = workflowId ~ ("{" ~> repsep(workflowInstruction, ",") <~ ",") ~ (goto <~ "}") ^^ {
    case id ~ instructions ~ fallback => Workflow(id, instructions, fallback)
  }

  val workflows: PackratParser[List[Workflow]] = rep(workflow)

  val xmasValue: PackratParser[XMASValue] = ("{" ~> repsep(xmas ~ ("=" ~> numberParser), ",") <~ "}") ^^ {
    case xmasValues => xmasValues.map { case xmas ~ value => xmas -> value }.toMap
  }

  val xmasValues: PackratParser[List[XMASValue]] = rep(xmasValue)

  val input: PackratParser[ParsedInput] = workflows ~ xmasValues ^^ {
    case workflows ~ xmasValues => ParsedInput(workflows, xmasValues)
  }
}

val rawInput = scala.io.Source.fromFile("./day19/input.txt").mkString
val maybeParsedInput = Parser.parseAll(Parser.input, rawInput)
val parsedInput = maybeParsedInput.get

val workflows: Map[WorkflowId, Workflow] = parsedInput.workflows.map { workflow =>
  workflow.id -> workflow
}.toMap

val initialWorkflow = workflows("in")

@tailrec
final def testFor(xmasValues: XMASValue, workflow: Workflow): Boolean =
  workflow.instructions.find { instruction =>
    val value = xmasValues(instruction.comparison.xmas)
    instruction.comparison.comparOp match {
      case ComparOp.LessThan => value < instruction.comparison.value
      case ComparOp.GreaterThan => value > instruction.comparison.value
    }
  } match {
    case Some(WorkflowInstruction(_, Goto.Accepted)) => true
    case Some(WorkflowInstruction(_, Goto.Rejected)) => false
    case Some(WorkflowInstruction(_, Goto.GotoWorkflow(id))) => testFor(xmasValues, workflows(id))
    case None => workflow.fallback match {
      case Goto.Accepted => true
      case Goto.Rejected => false
      case Goto.GotoWorkflow(id) => testFor(xmasValues, workflows(id))
    }
  }

val acceptedXMASValues = parsedInput.xmasValues.filter { xmasValues =>
  testFor(xmasValues, initialWorkflow)
}

val part1 = acceptedXMASValues.view.map(_.values.sum).sum

type Range = Range.Inclusive
val defaultRange: Range = 1 to 4000

type XMASRange = Map[XMAS, Range]

val defaultXMASRange: XMASRange = allXMAS.map(_ -> defaultRange).toMap

final def computeGotos(goto: Goto, xmasRanges: List[XMASRange]): List[XMASRange] =
  goto match {
    case Goto.Accepted => xmasRanges
    case Goto.Rejected => Nil
    case Goto.GotoWorkflow(id) => xmasRanges.flatMap(computeRangeWorkflow(workflows(id), _))
  }

final def splitCompareOp(comparOp: ComparOp, xmas: XMAS, value: Int, xmasRange: XMASRange): (XMASRange, XMASRange) =
  val range = xmasRange(xmas)
  comparOp match {
    case ComparOp.LessThan =>
      val trueRange = range.start to (value - 1)
      val falseRange = value to range.end
      val trueXMASRange = xmasRange + (xmas -> trueRange)
      val falseXMASRange = xmasRange + (xmas -> falseRange)
      trueXMASRange -> falseXMASRange
    case ComparOp.GreaterThan =>
      val trueRange = (value + 1) to range.end
      val falseRange = range.start to value
      val trueXMASRange = xmasRange + (xmas -> trueRange)
      val falseXMASRange = xmasRange + (xmas -> falseRange)
      trueXMASRange -> falseXMASRange
  }

final def splitCompareOpMultiple(comparOp: ComparOp, xmas: XMAS, value: Int, xmasRanges: List[XMASRange]): (List[XMASRange], List[XMASRange]) =
  val res = xmasRanges.map(splitCompareOp(comparOp, xmas, value, _))
  val trues = res.map(_._1)
  val falses = res.map(_._2)
  trues -> falses

final def computeWorkflowInstruction(instruction: WorkflowInstruction, xmasRanges: List[XMASRange]): (List[XMASRange], List[XMASRange]) =
  val (trueXMASRanges, falseXMASRanges) = splitCompareOpMultiple(instruction.comparison.comparOp, instruction.comparison.xmas, instruction.comparison.value, xmasRanges)
  val subXMASRanges = computeGotos(instruction.goto, trueXMASRanges)
  subXMASRanges -> falseXMASRanges

final def computeRangeWorkflow(workflow: Workflow, xmasRange: XMASRange): List[XMASRange] =
  var result: List[XMASRange] = Nil
  var remain = List(xmasRange)
  for {
    instruction <- workflow.instructions
  } {
    val (trueXMASRanges, falseXMASRanges) = computeWorkflowInstruction(instruction, remain)
    result = trueXMASRanges ++ result
    remain = falseXMASRanges
  }
  val fallback = computeGotos(workflow.fallback, remain)
  result = fallback ++ result
  result

final def sizeRange(xmasRange: XMASRange): Long = {
  //for {
  //  xmas <- allXMAS
  //  range = xmasRange(xmas)
  //} {
  //  print(s"$xmas: ${range.size} ")
  //}
  //println()
  xmasRange.values.toList.map(_.size.toLong).product
}

final def sizeRanges(xmasRanges: List[XMASRange]): Long =
  xmasRanges.map(sizeRange).sum

def part2Ranges = computeRangeWorkflow(workflows("in"), defaultXMASRange)

val part2 = sizeRanges(part2Ranges)