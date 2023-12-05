import $dep.`org.scala-lang.modules:scala-parser-combinators_3:2.3.0`
import scala.util.parsing.combinator._

case class LongRange(
  start: Long,
  end: Long
) {
  def contains(i: Long): Boolean =
    i >= start && i < end

  def intersects(other: LongRange): Boolean =
    contains(other.start) || contains(other.end - 1) || other.contains(start) || other.contains(end - 1)
}

case class Mapping(
  sourceRange: LongRange,
  delta: Long
)

enum Kind:
  case Seed
  case Soil
  case Fertilizer
  case Water
  case Light
  case Temperature
  case Humidity
  case Location

case class MappingLayer(
  from: Kind,
  to: Kind,
  mappings: List[Mapping]
)

case class InputMapping(
  seeds: List[Long],
  mappingLayers: List[MappingLayer]
)

object Parser extends RegexParsers with PackratParsers {
  val seeds: PackratParser[List[Long]] = "seeds:" ~> rep("\\d+".r) ^^ { _.map(_.toLong) }
  // 50 98 2
  val mapping: PackratParser[Mapping] = "\\d+".r ~ "\\d+".r ~ "\\d+".r ^^ {
    case destStart ~ sourceStart ~ rangeSize => {
      val destStartLong = destStart.toLong
      val sourceStartLong = sourceStart.toLong
      val rangeSizeLong = rangeSize.toLong
      val sourceRange = LongRange(sourceStartLong, sourceStartLong + rangeSizeLong)
      val delta = destStartLong - sourceStartLong
      Mapping(sourceRange, delta)
    }
  }

  val kind: PackratParser[Kind] = "seed" ^^^ Kind.Seed |
    "soil" ^^^ Kind.Soil |
    "fertilizer" ^^^ Kind.Fertilizer |
    "water" ^^^ Kind.Water |
    "light" ^^^ Kind.Light |
    "temperature" ^^^ Kind.Temperature |
    "humidity" ^^^ Kind.Humidity |
    "location" ^^^ Kind.Location

  val kindToKind: PackratParser[(Kind, Kind)] = kind ~ "-to-" ~ kind ^^ {
    case from ~ _ ~ to => (from, to)
  }

  val mappingLayer: PackratParser[MappingLayer] = (kindToKind <~ "map:") ~ rep(mapping) ^^ {
    case (from, to) ~ mappings => MappingLayer(from, to, mappings)
  }

  val inputMapping: PackratParser[InputMapping] = seeds ~ rep(mappingLayer) ^^ {
    case seeds ~ mappingLayers => InputMapping(seeds, mappingLayers)
  }
}

val rawInput = scala.io.Source.fromFile("./day05/input.txt").mkString
val maybeParsedInput = Parser.parseAll(Parser.inputMapping, rawInput)
val parsedInput = maybeParsedInput.get

def mapUsingLayer(layer: MappingLayer, input: List[Long]): List[Long] =
  val mappings = layer.mappings
  val from = layer.from
  val to = layer.to
  val result = input.map { i =>
    val mapping = mappings.find(_.sourceRange.contains(i))
    mapping match {
      case Some(mapping) => i + mapping.delta
      case None => i
    }
  }
  result

def useAllLayers(layers: List[MappingLayer], input: List[Long]): List[Long] =
  layers.foldLeft(input) { (acc, layer) =>
    mapUsingLayer(layer, acc)
  }

val seedLocations = useAllLayers(parsedInput.mappingLayers, parsedInput.seeds)
val part1 = seedLocations.min

def seedsToSeedRange(seeds: List[Long]): List[LongRange] =
  seeds.grouped(2).toList.map {
    case List(start, size) => LongRange(start, start + size - 1)
    case _ => throw new Exception("Odd number of seeds")
  }

def mapRangeUsingLayer(layer: MappingLayer, input: LongRange): List[LongRange] =
  val mappings = layer.mappings
  val applicableMappings = mappings.filter(_.sourceRange.intersects(input))
  val splitPoints = (applicableMappings.flatMap(m => List(Math.max(m.sourceRange.start, input.start), Math.min(m.sourceRange.end, input.end))) ++ List(input.start, input.end)).distinct
  val sortedSplitPoints = splitPoints.sorted
  val begins = sortedSplitPoints.take(sortedSplitPoints.length - 1)
  val ends = sortedSplitPoints.drop(1)
  val ranges = begins.zip(ends).map { case (begin, end) => LongRange(begin, end) }
  val mappedRanges = ranges.map { range =>
    val mapping = applicableMappings.find(_.sourceRange.intersects(range))
    mapping match {
      case Some(mapping) => LongRange(range.start + mapping.delta, range.end + mapping.delta)
      case None => range
    }
  }
  mappedRanges

def mapAllRangesUsingLayer(layer: MappingLayer, input: List[LongRange]): List[LongRange] =
  input.flatMap(mapRangeUsingLayer(layer, _))

def useAllLayersOnRanges(layers: List[MappingLayer], input: List[LongRange]): List[LongRange] =
  layers.foldLeft(input) { (acc, layer) =>
    mapAllRangesUsingLayer(layer, acc)
  }

val seedRanges = seedsToSeedRange(parsedInput.seeds)
val seedRangesLocations = useAllLayersOnRanges(parsedInput.mappingLayers, seedRanges)
val part2 = seedRangesLocations.map(_.start).min
  
