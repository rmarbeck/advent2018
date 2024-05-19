import scala.annotation.tailrec

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
def findTwice(toAdd: Vector[Int], currentFrequency: Int = 0, frequencies: NegativeSupportedBitSet = NegativeSupportedBitSet()): Int =
  val newFrequency = toAdd.head + currentFrequency
  frequencies.contains(newFrequency) match
    case true => newFrequency
    case false =>
      frequencies.add(newFrequency)
      findTwice(toAdd.tail :+ toAdd.head, newFrequency, frequencies)