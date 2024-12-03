import scala.annotation.{tailrec, targetName}
import scala.collection.mutable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (nbPlayers, lastMarble) = inputLines.head match
      case s"${nbPlayers} players; last marble is worth ${lastMarble} points" => (NbPlayers(nbPlayers.toInt), NbMarbles(lastMarble.toInt + 1))
      case _ => throw Exception("Not recognized")

    given NbPlayers = nbPlayers
    val resPart1 =
      given NbMarbles = lastMarble
      play(Player(1), Deque(), Map())

    val resPart2 =
      given NbMarbles = lastMarble * 100
      play(Player(1), Deque(), Map())


    val result1 = s"$resPart1"
    val result2 = s"$resPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def play(currentPlayer: Player, marbles: Game, scores: Map[Player, Score])(using NbPlayers, NbMarbles): Score =
  //println(marbles)
  marbles.getCurrent.value match
    case value if value > summon[NbMarbles].max => scores.values.max
    case value if value % 23 == 0 =>
      def rotate7BackAndPop: Marble =
        marbles.rotate7BackAndPop
      def addScore(marble1: Marble, marble2: Marble): Map[Player, Score] =
        scores.updatedWith(currentPlayer):
          case Some(currentValue) => Some(currentValue + marble1.value + marble2.value)
          case None => Some(marble1.value + marble2.value)
      val removed = marbles.rotate7BackAndPop
      val newScores = addScore(Marble(value), removed)
      play(currentPlayer.next, marbles, newScores)
    case value =>
      marbles.add2Away()
      play(currentPlayer.next, marbles, scores)


type Score = Long

trait Game:
  def rotate7BackAndPop: Marble
  def add2Away(): Unit
  def getCurrent: Marble

class Deque() extends Game:
  private val inner: mutable.ArrayDeque[Marble] = mutable.ArrayDeque[Marble](Marble(0))
  private var currentMarble: Marble = Marble(0).next

  def getCurrent: Marble = currentMarble

  @tailrec
  private def clockWise(times: Int): mutable.ArrayDeque[Marble] =
    times match
      case 0 => inner
      case _ =>
        inner.append(inner.removeHead())
        clockWise(times - 1)

  @tailrec
  private def counterClockWise(times: Int): mutable.ArrayDeque[Marble] =
    times match
      case 0 => inner
      case _ =>
        inner.prepend(inner.removeLast())
        counterClockWise(times - 1)

  override def rotate7BackAndPop: Marble =
    val extracted = counterClockWise(7).removeHead()
    currentMarble = currentMarble.next
    extracted

  override def add2Away(): Unit =
    clockWise(2).prepend(currentMarble)
    currentMarble = currentMarble.next

  override def toString: String =
    val zero = inner.indexOf(Marble(0))
    val start = inner.drop(zero).map(_.value).mkString(" ")
    val end = inner.take(zero).map(_.value).mkString(" ")
    s"$start $end"

case class Player(index: Int):
  def next(using NbPlayers): Player =
    if ((index + 1) > summon[NbPlayers].max)
      Player(1)
    else
      Player(index + 1)

case class Marble(value: Long):
  def next: Marble = Marble(value + 1)

case class NbPlayers(max: Int)

case class NbMarbles(max: Int):
  @targetName("multiply")
  def *(factor: Int): NbMarbles = NbMarbles(max * factor)

case class IndexOfCurrentMarble(index: Int):
  def nextCWIn(futureLength: Int): Int =
    index + 2 match
      case 2 if futureLength == 1 => 0
      case value if value == futureLength => 1
      case value => value
