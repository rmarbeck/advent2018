object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val stackPart1 = StackPart1()
    inputLines.head.split(" ").map(_.toInt).foreach(stackPart1.add)

    val resultPart1 = stackPart1.metaCounter

    val result1 = s"${resultPart1}"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

class StackPart1:
  var metaCounter: Int = 0
  private var data: List[(Int, Option[Int])] = List((1, Some(0)))
  private def popChild: Unit =
    data(1) match
      case (0, Some(0)) => data = Nil
      case (nbChildren, nbMeta) => data = (nbChildren - 1, nbMeta) +: data.drop(2)

  def add(newVal: Int): StackPart1 =
    data.headOption match
      case Some((0, Some(1))) =>
        metaCounter = metaCounter + newVal
        popChild
      case Some((0, Some(metaData))) =>
        metaCounter = metaCounter + newVal
        data = (0, Some(metaData-1)) +: data.tail
      case Some((_, Some(_))) =>
        data = (newVal, None) +: data
      case Some((nbChildren, None)) =>
        data = (nbChildren, Some(newVal)) +: data.tail
      case None => throw Exception("Not supposed to happen")

    this

/*class StackPart2:
  var metaCounter: Int = 0
  private var data: List[(Int, Option[Int], List[Int])] = List((1, Some(0), Nil))
  private def popChild: Unit =
    data(1) match
      case (0, Some(0)) => data = Nil
      case (nbChildren, nbMeta) => data = (nbChildren - 1, nbMeta) +: data.drop(2)

  def add(newVal: Int): StackPart2 =
    data.headOption match
      case Some((0, Some(1))) =>
        metaCounter = metaCounter + newVal
        popChild
      case Some((0, Some(metaData))) =>
        metaCounter = metaCounter + newVal
        data = (0, Some(metaData-1)) +: data.tail
      case Some((_, Some(_))) =>
        data = (newVal, None) +: data
      case Some((nbChildren, None)) =>
        data = (nbChildren, Some(newVal)) +: data.tail
      case None => throw Exception("Not supposed to happen")

    this*/