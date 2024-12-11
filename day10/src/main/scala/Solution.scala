import scala.annotation.{tailrec, targetName}

type Result = (Map[(Int, Int), Boolean], Int)

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val points = inputLines.collect:
      case s"position=<${x},${y}> velocity=<${vX},${vY}>" => Point.fromList(List(x, y, vX, vY).map(_.trim).map(_.toInt))

    val (message, counter) = moveUntilMessage(points, 0)

    val result1 = s"${display(message)}"
    val result2 = s"$counter"

    (s"$result1", s"$result2")


@tailrec
def moveUntilMessage(points: Seq[Point], counter: Int): Result =
  val mapOfPoints = points.map:
    case Point(Position(x, y), _) => (x, y) -> true
  .toMap.withDefaultValue(false)
  def isAMessage: Boolean =
    points.forall(_.around.count(mapOfPoints) >= 1)

  isAMessage match
    case true => (mapOfPoints, counter)
    case false => moveUntilMessage(points.map(_.next), counter + 1)


case class Position(x: Int, y: Int):
  @targetName("add")
  def +(velocity: Velocity): Position = Position(x + velocity.vX, y + velocity.vY)
case class Velocity(vX: Int, vY: Int)

case class Point(position: Position, velocity: Velocity):
  def next: Point = this.copy(position = position + velocity)
  def around: List[(Int, Int)] = List((0,-1),(0,1),(1,0),(1,-1),(1,1),(-1,0),(-1,-1),(-1,1)).map:
    case (diffX, diffY) => (position.x + diffX, position.y + diffY)

def display(points: Map[(Int, Int), Boolean]): String =
  val (startX, startY) = points.keys.head
  val (minX, maxX, minY, maxY) = points.tail.keys.foldLeft((startX,startX,startY,startY)):
    case ((minX, maxX, minY, maxY), (x, y)) => (Math.min(minX, x), Math.max(maxX, x), Math.min(minY, y), Math.max(maxY, y))

  (for
    y <- minY to maxY; x <- minX to maxX
  yield
    val before = x == minX match
      case true => "\n"
      case false => ""
    points((x, y)) match
      case true => before + "#"
      case false => before + "."
    ).mkString

object Point:
  def fromList(asList: List[Int]): Point =
    asList match
      case List(x, y, vX, vY) => Point(Position(x, y), Velocity(vX, vY))
      case _ => throw Exception("Not managed")