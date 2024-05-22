import scala.collection.immutable.BitSet
import scala.collection.mutable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val claims = inputLines.collect:
      case s"#$id @ $left,$top: ${width}x$height" =>
        val List(ci, cl, ct, cw, ch) = List(id, left, top, width, height).map(_.toInt)
        Claim(ci, (cl, ct), (cw, ch))

    val square = Square()
    claims.foreach(square.add)

    val resultPart1 = square.multipleClaims
    val resultPart2 = square.singleClaim

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

type LeftTop = (Int, Int)
type SquareSize = (Int, Int)

type Coords = (Int, Int)

class Square:
  import scala.collection.mutable.Map
  def multipleClaims: Int = content.count(_._2.size > 1)
  def singleClaim: String =
    val claimsIds = content.values.fold(BitSet.empty)(_ ++ _)
    content.values.foldLeft(claimsIds):
      case (acc, currentValue) if currentValue.size > 1 => acc -- currentValue
      case (acc, _ ) => acc
    .mkString(",")

  private val content: mutable.Map[Coords, Set[Int]] = mutable.Map()

  private def add(coords: Coords, claimId: Int): Square =
    content.update(coords, content.getOrElse(coords, Set.empty) + claimId)
    this

  def add(claim: Claim): Unit =
    for
      row <- claim.rows
      col <- claim.cols
    do
      add((row, col), claim.id)

case class Claim(id: Int, leftTop: LeftTop, size: SquareSize):
  lazy val rows: Seq[Int] = Iterator.from(leftTop._2).take(size._2).toSeq
  lazy val cols: Seq[Int] = Iterator.from(leftTop._1).take(size._1).toSeq