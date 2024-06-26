import scala.collection.parallel.*
import collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val coordinatesSeq = inputLines.collect:
      case s"$row, $col" => Coordinates(row.toInt, col.toInt)

    val pseudoInfiniteDistance = Math.max(size(coordinatesSeq.map(_.row)), size(coordinatesSeq.map(_.col)))

    val resultPart1 = calcAreaPart1(coordinatesSeq, pseudoInfiniteDistance)

    val maxTotal = coordinatesSeq.size match
      case 6 => 32
      case _ => 10000

    val resultPart2 = calcAreaPart2(guessMiddle(coordinatesSeq), coordinatesSeq, maxTotal =  maxTotal)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

def guessMiddle(in: Seq[Coordinates]): Coordinates =
  @tailrec
  def findMiddle(start: Coordinates, in: Seq[Coordinates]): Coordinates =
    val currentDistance = start.totalDistance(in)
    start.at(1).find(_.totalDistance(in) < currentDistance) match
      case Some(newMiddle) => findMiddle(newMiddle, in)
      case None => start
  val size = in.size
  val List(rowMiddle, colMiddle) = List[Coordinates => Int](c => c.row, c => c.col).map(in.map(_).sum).map(_ / size)

  val approximateMiddle = Coordinates(rowMiddle, colMiddle)
  findMiddle(approximateMiddle, in)

case class Coordinates(row: Int, col: Int):
  def totalDistance(to: Seq[Coordinates]): Int =
    to.map(distanceTo).sum

  private def distanceTo(other: Coordinates): Int = (row - other.row).abs + (col - other.col).abs

  def cardinalsAt(distance: Int): List[Coordinates] = List(-distance, distance).flatMap(drift => List(this.copy(row = row + drift), this.copy(col = col + drift)))

  def at(distance: Int): List[Coordinates] = {
    for
      r <- row - distance to row + distance
      c <- col - distance to col + distance
      if (r - row).abs + (c - col).abs == distance
    yield
      Coordinates(r, c)
  }.toList

  def isClosestTo(other: Coordinates, in: Seq[Coordinates]): Boolean =
    val minimal = distanceTo(other)
    !in.filterNot(_ == other).exists(curr => distanceTo(curr) <= minimal)

def calcAreaPart1(in: Seq[Coordinates], pseudoInfiniteDistance: Int): Int =
  def reachesInfiniteDistance(coordinates: Coordinates): Boolean =
    coordinates.cardinalsAt(pseudoInfiniteDistance).exists(_.isClosestTo(coordinates, in))
  def calcForOne(coordinates: Coordinates): Int =
    def calcByDistance(distance: Int = 1, current: Int = 1): Int =
      coordinates.at(distance).count(_.isClosestTo(coordinates, in)) match
        case 0 => current
        case value => calcByDistance(distance + 1, current + value)
    calcByDistance()

  in.par.filterNot(reachesInfiniteDistance).map(calcForOne).max

def calcAreaPart2(pseudoMiddle: Coordinates, in: Seq[Coordinates], currentDistance: Int = 1, counter: Int = 1, maxTotal: Int): Int =
  val next = pseudoMiddle.at(currentDistance)
  next.count(_.totalDistance(in) < maxTotal) match
    case 0 => counter
    case value => calcAreaPart2(pseudoMiddle, in, currentDistance + 1, counter + value, maxTotal)

def size(values: Seq[Int]): Int =
  val sorted = values.sorted
  sorted.last - sorted.head
