import Tile.*
import CrossStrategy.*

import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.TreeSet

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given Track = Track.from(inputLines)
    val carts = Carts.from(inputLines)

    val List(result1, result2) = runCarts(Carts.empty, carts).map(_.asResult)

    (s"$result1", s"$result2")

@tailrec
def runCarts(toMove: Carts, alreadyMoved: Carts, firstCrash: Option[Position] = None)(using Track): List[Position] =
  if (toMove.isEmpty)
      runCarts(alreadyMoved, alreadyMoved.tail, firstCrash)
  else
    val currentCart = toMove.head
    val nextCart = summon[Track].next(currentCart)
    alreadyMoved - currentCart.position + nextCart match
      case Right(newMoved) => runCarts(toMove.tail, newMoved, firstCrash)
      case Left(crash) =>
        crash.remainingCarts.size match
          case 0 | 1 => List(firstCrash.get, crash.remainingCarts.head.position)
          case _ =>
            val nextFirstCrash = firstCrash.orElse(Some(crash.position))
            runCarts(toMove.tail - crash.position, crash.remainingCarts, nextFirstCrash)


case class Crash(position: Position, remainingCarts: Carts)

class Carts(private val sortedCarts: TreeSet[Cart]):
  export sortedCarts.{size, head, isEmpty}
  def tail: Carts = Carts(sortedCarts.tail)
  @targetName("remove")
  def -(position: Position): Carts = Carts(sortedCarts.filterNot(_.position == position))
  @targetName("add")
  def +(cart: Cart): Either[Crash, Carts] =
    if (sortedCarts.toList.map(_.position).contains(cart.position))
      Left(Crash(cart.position, this - cart.position))
    else
      Right(Carts(sortedCarts + cart))


object Carts:
  def from(rawInput: Seq[String]): Carts =
    val foundCarts = rawInput.zipWithIndex.flatMap:
      (line, y) =>
        line.zipWithIndex.collect:
          case ('>', x) => Cart(Position(x, y), Direction.Right)
          case ('<', x) => Cart(Position(x, y), Direction.Left)
          case ('^', x) => Cart(Position(x, y), Direction.Up)
          case ('v', x) => Cart(Position(x, y), Direction.Down)

    Carts(TreeSet(foundCarts: _*))

  def from(cartsUnSorted: List[Cart]): Carts = new Carts(TreeSet.from(cartsUnSorted))

  def empty: Carts = Carts(TreeSet())

case class Position(x: Int, y: Int):
  def step(direction: Direction): Position =
    direction match
      case Direction.Up => this.copy(y = y - 1)
      case Direction.Down => this.copy(y = y + 1)
      case Direction.Left => this.copy(x = x - 1)
      case Direction.Right => this.copy(x = x + 1)

  def asResult: String = s"$x,$y"

case class Cart(position: Position, direction: Direction, strategy: CrossStrategy = CrossStrategy.default):
  def straightForward: Cart = Cart(position.step(direction), direction, strategy)
  def topRightCorner: Cart = turn(Direction.Up, Direction.Left)
  def topLeftCorner: Cart = turn(Direction.Up, Direction.Right)
  def bottomRightCorner: Cart = turn(Direction.Down, Direction.Left)
  def bottomLeftCorner: Cart = turn(Direction.Down, Direction.Right)

  def cross: Cart =
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