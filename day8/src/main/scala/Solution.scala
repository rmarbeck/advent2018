object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val stackPart1 = StackPart1()
    inputLines.head.split(" ").map(_.toInt).foreach(stackPart1.add)

    val resultPart1 = stackPart1.value

    val stackPart2 = StackPart2()
    inputLines.head.split(" ").map(_.toInt).foreach(stackPart2.add)

    val resultPart2 = stackPart2.value

    val result1 = s"${resultPart1}"
    val result2 = s"${resultPart2}"

    (s"${result1}", s"${result2}")

end Solution

class StackPart1:
  private var data: List[Node] = List(InitialRoot())
  def value: Int = data.headOption.map(_.getValue).getOrElse(0)

  private def popChild(value: Int): Unit =
    (data(1), data(0)) match
      case (parent, child) =>
        data = parent.popChild(child, value) +: data.drop(2)

  def add(newVal: Int): StackPart1 =
    data.head match
      case node: FullNode if !node.hasChildren && node.remainingMetaData == 1 =>
        popChild(newVal)
      case node: FullNode if !node.hasChildren =>
        data = node.computeMetaDataAndUpdate(newVal) +: data.tail
      case node: (FullNode | InitialRoot) =>
        data = PartialNode(newVal) +: data
      case partialNode: PartialNode =>
        data = FullNode.from(partialNode, newVal) +: data.tail

    this

trait Node:
  def hasChildren: Boolean = false
  def hasRemainingMetaData: Boolean = false
  def remainingMetaData: Int = 0
  def computeMetaDataAndUpdate(newValue: Int): Node  = throw Exception("Not supported")
  def popChild(child: Node, newValue: Int): Node = throw Exception("Not supported")
  def getValue: Int = throw Exception("Not supported")

case class ParsedRoot(value: Int) extends Node:
  override val getValue = value

case class InitialRoot() extends Node:
  override val hasChildren = true
  override def popChild(child: Node, newValue: Int): Node =
    child match
      case fullNode: FullNode => ParsedRoot(fullNode.value + newValue)
      case _ => throw Exception("Not supported")

case class PartialNode(nbChildren: Int) extends Node:
  override val hasChildren = nbChildren != 0

case class FullNode(nbChildren: Int, nbMedaData: Int, value: Int = 0) extends Node:
  override val hasChildren = nbChildren != 0
  override val hasRemainingMetaData = nbMedaData != 0
  override val remainingMetaData = nbMedaData
  override def computeMetaDataAndUpdate(newValue: Int): Node = this.copy(nbMedaData = nbMedaData - 1, value = value + newValue)
  override def popChild(child: Node, newValue: Int): FullNode =
    child match
      case fullNode: FullNode =>  this.copy(nbChildren = nbChildren - 1, value = value + fullNode.value + newValue)
      case _ => throw Exception("Not supported")
  override val getValue = value

object FullNode:
  def from(partialNode: PartialNode, nbMetaData: Int): FullNode = FullNode(partialNode.nbChildren, nbMetaData)


class StackPart2:
  private var data: List[NodePart2] = List(InitialRootPart2())
  def value: Int = data.headOption.map(_.getValue).getOrElse(0)

  private def popChild(value: Int): Unit =
    (data(1), data(0)) match
      case (parent, child) =>
        data = parent.popChild(child, value) +: data.drop(2)

  def add(newVal: Int): StackPart2 =
    data.head match
      case node: FullNodePart2 if !node.hasStillChildrenToParse && node.remainingMetaData == 1 =>
        popChild(newVal)
      case node: FullNodePart2 if !node.hasStillChildrenToParse =>
        data = node.computeMetaDataAndUpdate(newVal) +: data.tail
      case node: (FullNodePart2 | InitialRootPart2) =>
        data = PartialNodePart2(newVal) +: data
      case partialNode: PartialNodePart2 =>
        data = FullNodePart2.from(partialNode, newVal) +: data.tail

    this

trait NodePart2:
  def hasChildren: Boolean = false
  def hasStillChildrenToParse: Boolean = false
  def hasRemainingMetaData: Boolean = false
  def remainingMetaData: Int = 0
  def computeMetaDataAndUpdate(newValue: Int): NodePart2  = throw Exception("Not supported")
  def popChild(child: NodePart2, newValue: Int): NodePart2 = throw Exception("Not supported")
  def getValue: Int = throw Exception("Not supported")

case class ParsedRootPart2(value: Int) extends NodePart2:
  override val getValue = value

case class InitialRootPart2() extends NodePart2:
  override val hasChildren = true
  override val hasStillChildrenToParse: Boolean = true
  override val hasRemainingMetaData: Boolean = true
  override val remainingMetaData: Int = 1
  override def popChild(child: NodePart2, newValue: Int): NodePart2 =
    child match
      case fullNode: FullNodePart2 => ParsedRootPart2(child.computeMetaDataAndUpdate(newValue).getValue)
      case _ => throw Exception("Not supported")

case class PartialNodePart2(nbChildren: Int) extends NodePart2:
  override val hasChildren = nbChildren != 0

case class FullNodePart2(nbChildren: Int, nbMedaData: Int, values: List[Int] = Nil, value: Int = 0) extends NodePart2:
  override val hasChildren = nbChildren + values.length != 0
  override val hasStillChildrenToParse: Boolean = nbChildren != 0
  override val hasRemainingMetaData = nbMedaData != 0
  override val remainingMetaData = nbMedaData
  override def computeMetaDataAndUpdate(newValue: Int): NodePart2 =
    hasChildren match
      case false => this.copy(nbMedaData = nbMedaData - 1, value = value + newValue)
      case true =>
        newValue <= values.length match
          case true => this.copy(nbMedaData = nbMedaData - 1, value = value + values(newValue - 1))
          case false => this.copy(nbMedaData = nbMedaData - 1)

  override def popChild(child: NodePart2, newValue: Int): FullNodePart2 =
    this.copy(nbChildren = nbChildren - 1, values = values :+ child.computeMetaDataAndUpdate(newValue).getValue)

  override val getValue = value

object FullNodePart2:
  def from(partialNode: PartialNodePart2, nbMetaData: Int): FullNodePart2 = FullNodePart2(partialNode.nbChildren, nbMetaData)