import scala.annotation.tailrec
import $dep.`org.scala-lang.modules:scala-parser-combinators_3:2.3.0`
import scala.util.parsing.combinator._

// example input :
// broadcaster -> a
// %a -> inv, con
// &inv -> b
// %b -> con
// &con -> output

opaque type Identifier = String

type Connections = List[Identifier]

enum Kind:
  case Broadcaster
  case FlipFlop
  case Conjunction

final case class Component(id: Identifier, kind: Kind, connections: Connections)

final case class ParsedInput(components: List[Component])

object Parser extends RegexParsers with PackratParsers {
  val identifier: PackratParser[Identifier] = "[a-z]+".r ^^ { _.toString }

  val connections: PackratParser[Connections] = repsep(identifier, ",")

  val componentIdWithKind: PackratParser[(Kind, Identifier)] = (("%" | "&") ~ identifier) ^^ {
    case "%" ~ id => Kind.FlipFlop -> id
    case "&" ~ id => Kind.Conjunction -> id
  } | "broadcaster" ^^ { _ => Kind.Broadcaster -> "broadcaster" }

  val component: PackratParser[Component] = componentIdWithKind ~ ("->" ~> connections) ^^ {
    case (kind, id) ~ connections => Component(id, kind, connections)
  }

  val input: PackratParser[ParsedInput] = rep(component) ^^ { components =>
    ParsedInput(components)
  }
}

val rawInput = scala.io.Source.fromFile("./day20/input.txt").mkString
val maybeParsedInput = Parser.parseAll(Parser.input, rawInput)
val parsedInput = maybeParsedInput.get

enum Pulse:
  case HighPulse
  case LowPulse

  override def toString = this match
    case HighPulse => "high"
    case LowPulse => "low"

sealed trait Gate:
  def id: Identifier
  def kind: Kind
  def connections: Connections
  def state: List[Pulse]
  def react(pulse: Pulse, from: Identifier): Option[Pulse]

final case class BroadcasterGate(connections: Connections) extends Gate:
  val id = "broadcaster"
  val kind = Kind.Broadcaster
  val state = Nil
  def react(pulse: Pulse, from: Identifier): Option[Pulse] = Some(pulse)
  override def toString = s"broadcaster"

final class FlipFlopGate(val id: Identifier, val connections: Connections) extends Gate:
  val kind = Kind.FlipFlop

  private var innerState = Pulse.LowPulse

  def state = List(innerState)

  def react(pulse: Pulse, from: Identifier): Option[Pulse] = pulse match
    case Pulse.LowPulse =>
      innerState match
        case Pulse.HighPulse => innerState = Pulse.LowPulse
        case Pulse.LowPulse => innerState = Pulse.HighPulse
      Some(innerState)
    case Pulse.HighPulse => None

  override def toString = s"$id:flip($innerState)"

final class ConjunctionGate(val id: Identifier, val connections: Connections, val inbounds: Connections) extends Gate:
  val kind = Kind.Conjunction

  private var innerState = inbounds.map(_ => Pulse.LowPulse).toArray

  def state = innerState.toList

  def react(pulse: Pulse, from: Identifier): Option[Pulse] =
    val fromIndex = inbounds.indexOf(from)
    innerState(fromIndex) = pulse
    if innerState.forall(_ == Pulse.HighPulse) then
      Some(Pulse.LowPulse)
    else
      Some(Pulse.HighPulse)

  override def toString = s"$id:conj(${innerState.mkString(", ")})"

def buildGates(): Map[Identifier, Gate] = parsedInput.components.map { component =>
  component.kind match
    case Kind.Broadcaster => component.id -> BroadcasterGate(component.connections)
    case Kind.FlipFlop => component.id -> FlipFlopGate(component.id, component.connections)
    case Kind.Conjunction => component.id -> ConjunctionGate(component.id, component.connections, parsedInput.components.filter(_.connections.contains(component.id)).map(_.id))
}.toMap

case class Event(targetId: Identifier, senderId: Identifier, pulse: Pulse)

type PulseQueue = scala.collection.immutable.Queue[Event]

@tailrec
final def processQueue(queue: PulseQueue, gates: Map[Identifier, Gate], highCount: Long, lowCount: Long): (Long, Long) =
  queue.headOption match
    case Some(Event(targetId, senderId, pulse)) =>
      val gate = gates(targetId)
      val gateReaction = gate.react(pulse, senderId)
      val newPulses = gateReaction match
        case Some(newPulse) => gate.connections.map(Event(_, targetId, newPulse))
        case None => Nil
      //for {
      //  p <- newPulses
      //} {
      //  println(s"${p.senderId} -${p.pulse}-> ${p.targetId}")
      //}
      val newHighCount = if gateReaction.contains(Pulse.HighPulse) then highCount + newPulses.size else highCount
      val newLowCount = if gateReaction.contains(Pulse.LowPulse) then lowCount + newPulses.size else lowCount
      val newQueue = queue.tail ++ newPulses.filter(e => gates.contains(e.targetId))
      processQueue(newQueue, gates, newHighCount, newLowCount)
    case None => highCount -> lowCount

def pressButton(gates: Map[Identifier, Gate]): (Long, Long) =
  println("button -low-> broadcaster")
  val initialQueue = scala.collection.immutable.Queue(Event("broadcaster", "button", Pulse.LowPulse))
  processQueue(initialQueue, gates, 0L, 1L)

val part1Gates = buildGates()

val after1000Press = (1 to 1000).map(_ => pressButton(part1Gates)).reduce((a, b) => (a._1 + b._1) -> (a._2 + b._2))
val part1 = after1000Press._1 * after1000Press._2

val part2Gates = buildGates()

@tailrec
final def processQueuePart2(queue: PulseQueue, gates: Map[Identifier, Gate]): Boolean =
  queue.headOption match
    case Some(Event(targetId, senderId, pulse)) =>
      val gate = gates(targetId)
      val gateReaction = gate.react(pulse, senderId)
      val newPulses = gateReaction match
        case Some(newPulse) => gate.connections.map(Event(_, targetId, newPulse))
        case None => Nil
      if (gateReaction.contains(Pulse.LowPulse) && newPulses.exists(_.targetId == "rx")) then
        true
      else
        val newQueue = queue.tail ++ newPulses.filter(e => gates.contains(e.targetId))
        processQueuePart2(newQueue, gates)
    case None => false

def pressButtonPart2(gates: Map[Identifier, Gate]): Boolean =
  val initialQueue = scala.collection.immutable.Queue(Event("broadcaster", "button", Pulse.LowPulse))
  processQueuePart2(initialQueue, gates)

// would run for a too long time :(

//var part2Count = 0L
//while (!pressButtonPart2(part2Gates)) do
//  part2Count += 1
//
//println(s"part2: $part2Count")