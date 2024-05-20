import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (double, triple) = inputLines.foldLeft((0, 0)):
      case ((double, triple), newStr) =>
        given ByFrequency = newStr.groupedByFrequency
        (double + toAdd(2), triple + toAdd(3))

    val resultPart1 = double * triple

    val resultPart2 = inputLines.combinations(2).flatMap(findBox).next()

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

def findBox(boxes: Seq[String]): Option[String] =
  boxes match
    case first :: second :: Nil => first matching second
    case _ => None

type ByFrequency = Map[Char, Int]

extension (str: String)
  def groupedByFrequency: ByFrequency = str.groupMapReduce(identity)(_ => 1)(_ + _)
  def matching(other: String): Option[String] =
    matches(str, other, 0, StringBuilder(str.length))

extension (bool: Boolean)
  def toInt: Int = if (bool) 1 else 0

def toAdd(freq: Int)(using ByFrequency): Int = summon[ByFrequency].exists(_._2 == freq).toInt

@tailrec
def matches(first: String, second: String, currentCounter: Int = 0, output: StringBuilder): Option[String] =
  require(first.length == second.length)

  currentCounter > 1 match
    case true => None
    case false =>
      first.headOption match
        case None =>
          currentCounter match
            case 1 => Some(output.toString())
            case _ => None
        case Some(currentHead) =>
          currentHead == second.head match
            case true => matches(first.tail, second.tail, currentCounter, output.append(currentHead))
            case false => matches(first.tail, second.tail, currentCounter + 1, output)