import scala.annotation.tailrec
import scala.collection.parallel.*

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val input = inputLines.head

    //val synthesized = synthesize2(StringBuilder(input))

    val stack = Stack()
    inputLines.head.foreach(stack.add)

    val resultPart1 = stack.size

    val synthesized = stack.toString

    import collection.parallel.CollectionConverters.SetIsParallelizable
    val resultPart2 = synthesized.map(_.toLower).toSet.par.map:
      currentLetter =>
        val newInput = StringBuilder(synthesized.filter(cur => cur.toLower != currentLetter))
        synthesize(newInput).length
    .min

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"


    (s"${result1}", s"${result2}")

end Solution

class Stack:
  override def toString: String = data.mkString
  def size: Int = data.length
  var data : Vector[Char] = Vector.empty
  def add(char: Char): Stack =
    if (data.lastOption.map(current => (current.toInt - char.toInt).abs == 32).getOrElse(false))
      data = data.init
    else
      data = data :+ char
    this


@tailrec
def synthesize(input: StringBuilder): String =
  val position = input.sliding(2, 1).zipWithIndex.find:
    case (current, index) if current(0).toLower == current(1).toLower && current(0) != current(1) => true
    case _ => false
  .map(_._2)
  position match
    case None => input.toString()
    case Some(position) =>
      val newInput = input.replace(position, position + 2, "")
      synthesize(newInput)

@tailrec
def synthesize2(input: StringBuilder, lastPosition: Int = 0): String =
  val position = input.toString.drop(lastPosition).sliding(2, 1).zipWithIndex.find:
    case (current, index) if current.length == 2 && current(0).toLower == current(1).toLower && current(0) != current(1) => true
    case _ => false
  .map(_._2 + lastPosition)
  (position, lastPosition) match
    case (None, 0) => input.toString
    case (None, _) => synthesize2(input, 0)
    case (Some(position), _) =>
      val newInput = input.replace(position, position + 2, "")
      synthesize2(newInput, position)