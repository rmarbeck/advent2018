import scala.collection.mutable
import scala.collection.parallel._

type GridSerial = Int
type SummedAreaTable = Map[(Int, Int), Int]
val gridSize: Int = 300

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given GridSerial = inputLines.head.toInt

    given SummedAreaTable = initializeSummedAreaTable

    val resultPart1 = findFromSummedAreaTable(3, 3)

    val resultPart2 = findFromSummedAreaTable(1, gridSize)

    val result1 = s"${resultPart1.take(2).mkString(",")}"
    val result2 = s"${resultPart2.mkString(",")}"

    (s"$result1", s"$result2")

def initializeSummedAreaTable(using gridSerial: GridSerial): SummedAreaTable =
  val data: mutable.Map[(Int, Int), Int] = mutable.Map().withDefaultValue(0)

  val indices = 1 to gridSize
  for
    x <- indices; y <- indices
  do
    data.update((x, y), score(x, y) + data(x - 1, y) + data(x, y - 1) - data(x - 1, y - 1))

  data.toMap.withDefaultValue(0)

def findFromSummedAreaTable(sizeMin: Int, sizeMax: Int)(using summedArea: SummedAreaTable): List[Int] =
  def findBest(currentSize: Int): (Int, Int, Int, Int) =
    val (bestX, bestY) -> bestScore =
      val indices = 1 to gridSize - currentSize + 1
      (for
        x <- indices; y <- indices
        List(xMax, yMax) = List(x, y).map(_ + currentSize - 1)
      yield
        val topLeftOutside        = (x - 1, y - 1)
        val bottomLeftOutside     = (x - 1,  yMax)
        val bottomLRightIncluded  = (xMax ,  yMax)
        val topRightOutside       = (xMax , y - 1)

        (x, y) -> (summedArea(bottomLRightIncluded) + summedArea(topLeftOutside) - summedArea(bottomLeftOutside) - summedArea(topRightOutside))
      ).maxBy(_._2)

    (bestX, bestY, bestScore, currentSize)

  import collection.parallel.CollectionConverters.RangeIsParallelizable
  val (bestX, bestY, _ , size) = (sizeMin to sizeMax).par.map(findBest).maxBy(_._3)

  List(bestX, bestY, size)


def score(x: Int, y: Int)(using gridSerial: GridSerial): Int =
  val rackId = x + 10
  val base = ((rackId * y) + gridSerial) * rackId match
    case value if value >= 100 => value.toString.dropRight(2).last.asDigit
    case _ => 0

  base - 5
