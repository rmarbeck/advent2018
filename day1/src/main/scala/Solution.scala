import scala.annotation.tailrec
import scala.collection.mutable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val drifts = inputLines.map(_.toInt).toVector

    val resultPart1 = drifts.sum
    val resultPart2 = findTwice(drifts)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def findTwice(toAdd: Vector[Int], currentFrequency: Int = 0, positiveFrequencies: mutable.BitSet = mutable.BitSet(), negativeFrequencies: mutable.BitSet = mutable.BitSet()): Int =
  val newFrequency = toAdd.head + currentFrequency
  val foundTwice = newFrequency match
    case value if value >= 0 => positiveFrequencies.contains(newFrequency)
    case _ => negativeFrequencies.contains(- newFrequency)

  foundTwice match
    case true => newFrequency
    case false =>
      newFrequency match
        case value if value >= 0 => positiveFrequencies.add(newFrequency)
        case _ => negativeFrequencies.add(- newFrequency)
      findTwice(toAdd.tail :+ toAdd.head, newFrequency, positiveFrequencies, negativeFrequencies)
