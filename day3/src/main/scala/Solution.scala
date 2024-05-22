object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val claims = inputLines.collect:
      case s"#$id @ $left,$top: ${width}x$height" => Claim(id, (left.toInt, top.toInt), (width.toInt, height.toInt))



    //
    // Code is here
    //

    /**
     * #1 @ 1,3: 4x4
     * #2 @ 3,1: 4x4
     * #3 @ 5,5: 2x2
     */

    val result1 = s""
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

type LeftTop = (Int, Int)
type SquareSize = (Int, Int)

case class Claim(id: String, leftTop: LeftTop, size: SquareSize)