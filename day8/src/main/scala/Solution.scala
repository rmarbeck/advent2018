object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (stackPart1, stackPart2) = (Stack(using Part1), Stack(using Part2))
    inputLines.head.split(" ").map(_.toInt).foreach:
      input =>
        stackPart1.add(input)
        stackPart2.add(input)

    val resultPart1 = stackPart1.value
    val resultPart2 = stackPart2.value

    val result1 = s"${resultPart1}"
    val result2 = s"${resultPart2}"

    (s"${result1}", s"${result2}")

end Solution

enum Behaviour:
  case Part1, Part2

export Behaviour.*

class Stack(using behaviour: Behaviour):
  private var data: List[Node] = List(InitialRoot())

  def value: Int = data.headOption.map(_.getValue).getOrElse(0)

  private def popChild(value: Int): Unit =
    (data(1), data(0)) match
      case (parent, child) =>
        data = parent.popChild(child, value) +: data.drop(2)

  def add(newVal: Int): Unit =
    data.head match
      case node: FullNode if node.isParsingLastMetaData =>
        popChild(newVal)
      case node: FullNode if node.isParsingMetaData =>
        data = node.computeMetaDataAndUpdate(newVal) +: data.tail
      case node: (FullNode | InitialRoot) =>
        data = PartialNode(newVal) +: data
      case partialNode: PartialNode =>
        data = FullNode.from(partialNode, newVal) +: data.tail

trait Node:
  def hasChildren: Boolean = false
  def hasStillChildrenToParse: Boolean = false
  def hasRemainingMetaData: Boolean = false
  def remainingMetaDataNumber: Int = 0
  def computeMetaDataAndUpdate(newValue: Int): Node  = throw Exception("Not supported")
  def popChild(child: Node, newValue: Int): Node = throw Exception("Not supported")
  def getValue: Int = throw Exception("Not supported")

case class ParsedRoot(value: Int)(using Behaviour) extends Node:
  override val getValue = value

case class InitialRoot()(using Behaviour) extends Node:
  override val hasChildren = true
  override val hasStillChildrenToParse: Boolean = true
  override val hasRemainingMetaData: Boolean =
    summon[Behaviour] match
      case Part1 => false
      case Part2 => true
  override val remainingMetaDataNumber: Int =
    summon[Behaviour] match
      case Part1 => 0
      case Part2 => 1
  override def popChild(child: Node, newValue: Int): Node = ParsedRoot(child.computeMetaDataAndUpdate(newValue).getValue)

case class PartialNode(nbChildren: Int)(using Behaviour) extends Node:
  override val hasChildren = nbChildren != 0

case class FullNode(nbChildren: Int, nbMedaData: Int, values: Vector[Int] = Vector.empty, value: Int = 0)(using Behaviour) extends Node:
  def isParsingLastMetaData: Boolean = !hasStillChildrenToParse && remainingMetaDataNumber == 1
  def isParsingMetaData: Boolean = !hasStillChildrenToParse
  override val hasChildren = nbChildren + values.length != 0
  override val hasStillChildrenToParse: Boolean = nbChildren != 0
  override val hasRemainingMetaData = nbMedaData != 0
  override val remainingMetaDataNumber = nbMedaData
  override def computeMetaDataAndUpdate(newValue: Int): Node =
    (summon[Behaviour], hasChildren) match
      case (Part1, _) | (Part2, false) =>
        this.copy(nbMedaData = nbMedaData - 1, value = value + newValue)
      case _ =>
        newValue <= values.length match
          case true => this.copy(nbMedaData = nbMedaData - 1, value = value + values(newValue - 1))
          case false => this.copy(nbMedaData = nbMedaData - 1)

  override def popChild(child: Node, newValue: Int): FullNode =
    val childValue = child.computeMetaDataAndUpdate(newValue).getValue
    this.copy(nbChildren = nbChildren - 1, value = value + childValue)
    summon[Behaviour] match
      case Part1 =>
        this.copy(nbChildren = nbChildren - 1, value = value + childValue)
      case Part2 =>
        this.copy(nbChildren = nbChildren - 1, values = values :+ childValue)

  override val getValue = value

object FullNode:
  def from(partialNode: PartialNode, nbMetaData: Int)(using Behaviour): FullNode = FullNode(partialNode.nbChildren, nbMetaData)
