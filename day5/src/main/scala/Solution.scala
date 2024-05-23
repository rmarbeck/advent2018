
final val DIFFERENCE_BETWEEN_LOWER_AND_UPPER_CASE: Int = 32

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val synthesized = synthesizer(inputLines.head.iterator)

    val resultPart1 = synthesized.length

    val resultPart2 = synthesized.map(_.toLower).toSet.map:
      currentLetter =>
        synthesizer(synthesized.iterator.filterNot(_.toLower == currentLetter)).length
    .min

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

class Stack:
  override def toString: String = data.mkString
  def size: Int = data.length
  private var data : Vector[Char] = Vector.empty
  def add(char: Char): Stack =
    if (data.lastOption.exists(current => (current.toInt - char.toInt).abs == DIFFERENCE_BETWEEN_LOWER_AND_UPPER_CASE))
      data = data.init
    else
      data = data :+ char
    this

def synthesizer(input: Iterator[Char]): String =
  val stack = Stack()
  input.foreach(stack.add)
  stack.toString