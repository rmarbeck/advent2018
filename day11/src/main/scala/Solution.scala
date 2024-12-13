import scala.collection.mutable
import scala.collection.parallel._

type Grid = Map[(Int, Int), Int]
type GridSerial = Int

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given GridSerial = inputLines.head.toInt

    given scores: Grid =
      (for
        x <- 1 to 300; y <- 1 to 300
      yield
        (x, y) -> score(x, y)
        ).toMap

    val result = findBest(3, 3)

    val resultPart2 = findBest(1, 300)

    val result1 = s"${result._1},${result._2}"
    val result2 = s"${resultPart2._1},${resultPart2._2},${resultPart2._3}"

    (s"$result1", s"$result2")

def findBest(sizeMin: Int, sizeMax: Int)(using grid: Grid): (Int, Int, Int) =
  def findBest(size: Int): ((Int, Int), Int) =
    val (deltaMin, deltaMax): (Int, Int) = size match
      case value if value % 2 == 0 => ((size / 2) - 1, size / 2)
      case value => (size / 2, size / 2)
    (for
      x <- 1 + deltaMin to 300 - deltaMax; y <- 1 + deltaMin to 300 - deltaMax
    yield
      val (topLeft, score) = Cache.scoreCached(x, y, size)
      topLeft -> score
    ).maxBy(_._2)

  val (((bestX, bestY), value), size) =
    Range(sizeMin, sizeMax + 1).map:
      size =>
        print(".")
        (findBest(size), size)
    .maxBy(_._2)

  (bestX, bestY, size)


object Cache:
  val cacheForSquareList: mutable.Map[Int, List[(Int, Int)]] = mutable.Map()
  val cacheForScores: mutable.Map[(Int, Int, Int), ((Int, Int), Int)] = mutable.Map()

  def scoreCached(x: Int, y: Int, size: Int)(using grid: Grid): ((Int, Int), Int) =
    cacheForScores.getOrElseUpdate((x, y, size), around(x, y)(size))

  def squareCached(size: Int): List[(Int, Int)] =
    cacheForSquareList.getOrElseUpdate(size, square(size))

def square(size: Int): List[(Int, Int)] =
  val (deltaMin, deltaMax): (Int, Int) = size match
    case value if value % 2 == 0 => ((size / 2) - 1, size / 2)
    case value => (size / 2, size / 2)

  (for x <- -deltaMin to deltaMax; y <- -deltaMin to deltaMax
  yield (x, y)
   ).toList

def square(pseudoCenter: (Int, Int), size: Int): List[(Int, Int)] =
  val (deltaMin, deltaMax): (Int, Int) = size match
    case value if value % 2 == 0 => ((size / 2) - 1, size / 2)
    case value => (size / 2, size / 2)

  (for
    x <- (pseudoCenter._1 - deltaMin) to (pseudoCenter._1 + deltaMax)
    y <- (pseudoCenter._2 - deltaMin) to (pseudoCenter._2 + deltaMax)
  yield
    (x, y)
    ).toList

def around(x: Int, y: Int)(size: Int)(using grid: Grid): ((Int, Int), Int) =
  size match
    case 1 => ((x, y) ,grid(x,y))
    case _ =>
      size % 2 match
        case 0 =>
          val row =
            for
              valX <- (x - size / 2) + 1 to x + size / 2
            yield
              grid(valX, y + size / 2)
          val col =
            for
              valY <- (y - size / 2) + 1 to y + size / 2
            yield
              grid(x + size / 2, valY)

          (((x - size / 2) + 1, (y - size / 2) + 1),  Cache.scoreCached(x, y, size - 1)._2 + row.sum + col.sum)

        case _ =>
          val row =
            for
              valX <- (x - size / 2) to x + size / 2
            yield
              grid(valX, y - size / 2)
          val col =
            for
              valY <- (y - size / 2) to y + size / 2
            yield
              grid(x - size / 2, valY)

          ((x - size / 2, y - size / 2), Cache.scoreCached(x, y, size - 1)._2 + row.sum + col.sum)


def score(x: Int, y: Int)(using gridSerial: GridSerial): Int =
  val rackId = x + 10
  val base = ((rackId * y) + gridSerial) * rackId match
    case value if value >= 100 => value.toString.dropRight(2).last.asDigit
    case  _=> 0

  base - 5
