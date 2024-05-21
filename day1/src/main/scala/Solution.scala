import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val drifts = inputLines.map(_.toInt)

    val resultPart1 = drifts.sum
    val resultPart2 = findTwiceAlt(CyclicIterator(drifts.iterator))

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

@tailrec
def findTwiceAlt(toAdd: Iterator[Int], currentFrequency: Int = 0, frequencies: NegativeSupportedBitSet = NegativeSupportedBitSet()): Int =
  val newFrequency = toAdd.next() + currentFrequency
  frequencies.contains(newFrequency) match
    case true => newFrequency
    case false =>
      frequencies.add(newFrequency)
      findTwiceAlt(toAdd, newFrequency, frequencies)


class CyclicIterator(input: Iterator[Int]) extends Iterator[Int]:
  private var isFullyKnown: Boolean = false
  private var fullCycle: Vector[Int] = Vector.empty
  private var cycleSize: Int = 0
  private var cursor: Int = 0
  override def hasNext: Boolean = true

  override def next(): Int =
    isFullyKnown match
      case true =>
        val current = fullCycle(cursor)
        cursor = (cursor + 1) % cycleSize
        current
      case false =>
        input.hasNext match
          case true =>
            val current = input.next()
            fullCycle = fullCycle :+ current
            current
          case false =>
            cycleSize = fullCycle.length
            isFullyKnown = true
            this.next()