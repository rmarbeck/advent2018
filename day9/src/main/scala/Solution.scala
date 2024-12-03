import scala.annotation.{tailrec, targetName}
import scala.collection.mutable

val winningMarbleModulo = 23
val part2Factor = 100

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (nbPlayers, lastMarble) = inputLines.head match
      case s"${nbPlayers} players; last marble is worth ${lastMarble} points" => (NbPlayers(nbPlayers.toInt), NbMarbles(lastMarble.toInt + 1))
      case _ => throw Exception("Not recognized")

    given NbPlayers = nbPlayers
    val resPart1 =
      given NbMarbles = lastMarble
      play()

    val resPart2 =
      given NbMarbles = lastMarble * part2Factor
      play()

    val result1 = s"$resPart1"
    val result2 = s"$resPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def play(currentPlayer: Player = Player.First, marbles: Game = Deque(), scores: Map[Player, Score] = Map())(using NbPlayers, NbMarbles): Score =
  marbles.getCurrentMarble match
    case marble if marble.value > summon[NbMarbles].max => scores.values.max
    case marble if marble.value % winningMarbleModulo == 0 =>
      def addScore(marble1: Marble, marble2: Marble): Map[Player, Score] =
        scores.updatedWith(currentPlayer):
          case Some(currentValue) => Some(marble1 + marble2 + currentValue)
          case None => Some(marble1 + marble2)
      val (newMarbles, removed) = marbles.rotate7BackAndPop()
      val newScores = addScore(marble, removed)
      play(currentPlayer.next, newMarbles, newScores)
    case _ =>
      val newMarbles = marbles.add2Away()
      play(currentPlayer.next, newMarbles, scores)


type Score = Long

trait Game:
  def rotate7BackAndPop(): (Game, Marble)
  def add2Away(): Game
  def getCurrentMarble: Marble

class Deque() extends Game:
  private val inner: mutable.ArrayDeque[Marble] = mutable.ArrayDeque[Marble](Marble.initial)
  private var currentMarble: Marble = Marble.firstPlayable

  def getCurrentMarble: Marble = currentMarble

  private def clockWise(times: Int): mutable.ArrayDeque[Marble] =
    doItNTimes(times){
      inner.append(inner.removeHead())
    }

  private def counterClockWise(times: Int): mutable.ArrayDeque[Marble] =
    doItNTimes(times){
      inner.prepend(inner.removeLast())
    }

  @tailrec
  private def doItNTimes(times: Int)(updater: => Unit): mutable.ArrayDeque[Marble] =
    times match
      case 0 => inner
      case _ =>
        updater
        doItNTimes(times - 1)(updater)

  override def rotate7BackAndPop(): (Game, Marble) =
    val extracted = counterClockWise(7).removeHead()
    currentMarble = currentMarble.next
    (this, extracted)

  override def add2Away(): Game =
    clockWise(2).prepend(currentMarble)
    currentMarble = currentMarble.next
    this


case class Player(index: Int):
  def next(using players: NbPlayers): Player =
    index + 1 match
      case value if value > players.max => Player(1)
      case value => Player(value)

object Player:
  lazy val First: Player = new Player(1)

case class Marble(value: Long):
  def next: Marble = Marble(value + 1)
  @targetName("add")
  def +(other: Marble): Long = value + other.value
  @targetName("add")
  def +(longValue: Int): Long = value + longValue

object Marble:
  lazy val initial: Marble = Marble(0)
  lazy val firstPlayable: Marble = Marble(1)

case class NbPlayers(max: Int)

case class NbMarbles(max: Int):
  @targetName("multiply")
  def *(factor: Int): NbMarbles = NbMarbles(max * factor)