import Tile.*
import Direction.*
import CrossStrategy.*

import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.TreeSet

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given Track = Track.from(inputLines)
    val carts = Carts.from(inputLines)

    val List(result1, result2) = runCarts(carts, None).map(_.asResult)

    (s"$result1", s"$result2")


@tailrec
def runCarts(carts: Carts, firstCrash: Option[Position])(using Track): List[Position] =
  val resultingCarts =
    carts.sortedCarts.toList.map:
      summon[Track].next
    .groupBy(_.position)

  val (crashing, notCrashing) = resultingCarts.partition(_._2.size >= 2)
  crashing.size match
    case 0 => runCarts(Carts.from(notCrashing.values.toList.flatten), firstCrash)
    case 1 =>
      println(s"Crash at ${crashing.keys}, remaining ${notCrashing.size}")
      notCrashing.size match
        case 0 => List(crashing.keys.head, Position(0, 0))
        case 1 => List(firstCrash.get, notCrashing.keys.head)
        case _ =>
          val nextFirstCrash = firstCrash.orElse(crashing.keys.headOption)
          runCarts(Carts.from(notCrashing.values.toList.flatten), firstCrash)
    case _ =>
      throw Exception("Not Supported")


case class Carts(sortedCarts: TreeSet[Cart]):
  def add(cart: Cart): Carts = Carts(sortedCarts + cart)
  def remove(cart: Cart): Carts = Carts(sortedCarts.filterNot(_ == cart))

object Carts:
  def from(rawInput: Seq[String]): Carts =
    val foundCarts = rawInput.zipWithIndex.flatMap:
      (line, y) =>
        line.zipWithIndex.collect:
          case ('>', x) => Cart(Position(x, y), Right)
          case ('<', x) => Cart(Position(x, y), Left)
          case ('^', x) => Cart(Position(x, y), Up)
          case ('v', x) => Cart(Position(x, y), Down)

    Carts(TreeSet(foundCarts: _*))

  def from(cartsUnSorted: List[Cart]): Carts = new Carts(TreeSet(cartsUnSorted: _*))

  def empty: Carts = Carts(TreeSet())

case class Position(x: Int, y: Int):
  def step(direction: Direction): Position =
    direction match
      case Up => this.copy(y = y - 1)
      case Down => this.copy(y = y + 1)
      case Left => this.copy(x = x - 1)
      case Right => this.copy(x = x + 1)

  def asResult: String = s"$x,$y"

case class Cart(position: Position, direction: Direction, strategy: CrossStrategy = CrossStrategy.default):
  lazy val straightForward: Cart = this.copy(position = position.step(direction))
  lazy val topRightCorner: Cart = turn(Up, Left)
  lazy val topLeftCorner: Cart = turn(Up, Right)
  lazy val bottomRightCorner: Cart = turn(Down, Left)
  lazy val bottomLeftCorner: Cart = turn(Down, Right)

  lazy val cross: Cart =
    strategy match
      case TurnLeft => Cart(position.step(direction), direction.turnLeft, strategy.next)
      case GoStraight => Cart(position.step(direction), direction, strategy.next)
      case TurnRight => Cart(position.step(direction), direction.turnRight, strategy.next)

  private def turn(nominalDir: Direction, finalDir: Direction): Cart =
    direction match
      case value if value == nominalDir => Cart(position.step(nominalDir), finalDir, strategy)
      case _ => Cart(position.step(!finalDir), !nominalDir, strategy)

object Cart:
  given ordering: Ordering[Cart] = Ordering.by(cart => (cart.position.y, cart.position.x))

enum CrossStrategy:
  case TurnLeft, GoStraight, TurnRight

  def next: CrossStrategy = CrossStrategy.fromOrdinal((this.ordinal + 1) % 3)

object CrossStrategy:
  val default: CrossStrategy = TurnLeft

enum Direction:
  case Up, Right, Down, Left

  def turnLeft: Direction = Direction.fromOrdinal((this.ordinal + 3) % 4)
  def turnRight: Direction = Direction.fromOrdinal((this.ordinal + 1) % 4)

extension (direction: Direction)
  @targetName("not")
  def unary_! : Direction = Direction.fromOrdinal((direction.ordinal + 2) % 4)

enum Tile:
  case UpDown, RightLeft, Crossing, TopLeft, TopRight, BottomRight, BottomLeft, Empty

object Tile:
  def from(char: Char, nextChar: Char): Tile =
    (char, nextChar) match
      case ('|' | 'v' | '^', _) => UpDown
      case ('-' | '<' | '>', _) => RightLeft
      case ('+', _) => Crossing
      case ('/', '-' | '+' | '<' | '>') => TopLeft
      case ('/', _) => BottomRight
      case ('\\', '-' | '+' |  '<' | '>') => BottomLeft
      case ('\\', _) => TopRight
      case _ => Empty

class Track private (data: Array[Array[Tile]]):
  private def value(at: Position): Tile = data(at.x)(at.y)

  def next(from: Cart): Cart =
    val Cart(currentPosition, currentDirection, strategy) = from
    val naturalNextPosition = currentPosition.step(currentDirection)
    value(naturalNextPosition) match
      case UpDown | RightLeft => from.straightForward
      case TopRight => from.topRightCorner
      case TopLeft => from.topLeftCorner
      case BottomRight => from.bottomRightCorner
      case BottomLeft => from.bottomLeftCorner
      case Crossing => from.cross
      case _ => throw Exception(s"Not supported - ${from} -> ${naturalNextPosition}")

object Track:
  def from(rawInput: Seq[String]): Track =
    val wideness = rawInput.map(_.length).max
    new Track(
      rawInput.toArray.map(
        currentLine => s"${currentLine}".padTo(wideness+1, ' ').sliding(2).toArray.map:
          current => Tile.from(current(0), current(1))
      )
     .transpose
    )
