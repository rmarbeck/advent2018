object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val sortedInputs = inputLines.sortBy:
      case s"[$time] $info" => time
      case _ => throw Exception("Not supported")

    val agenda = Agenda()

    sortedInputs.foreach:
      case s"[$time] Guard #$id begins shift" => agenda.setCurrentGuard(id)
      case s"[$date $hour:$minutes] falls asleep" => agenda.addSleep(date, minutes.toInt)
      case s"[$date $hour:$minutes] wakes up" => agenda.addWakeUp(date, minutes.toInt)

    val guardsStatistics = agenda.guards.map(_.statistics)

    val (bestGuardId, _ , _, bestMinute) = guardsStatistics.maxBy(_._2)
    val resultPart1 = bestGuardId * bestMinute

    val (bestGuardIdPart2, _, _, bestMinutePart2) = guardsStatistics.maxBy(_._3)
    val resultPart2 = bestGuardIdPart2 * bestMinutePart2

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

class Agenda:
  def guards = guardsByName.values
  import scala.collection.mutable.Map
  private val guardsByName: Map[String, Guard] = Map()
  private var currentGuard: Option[Guard] = None
  def setCurrentGuard(guardId: String): Unit = currentGuard = Some(guardsByName.getOrElseUpdate(guardId, Guard(guardId)))
  def addSleep(day: String, minute: Int): Unit = currentGuard.foreach(_.addSleep(day, minute))
  def addWakeUp(day: String, minute: Int): Unit = currentGuard.foreach(_.addWakeUp(day, minute))

type ASleepPeriod = (Int, Int)

class Guard(val id: String):
  import scala.collection.mutable.Map
  private val days: Map[String, List[ASleepPeriod]] = Map()
  private var currentGettingAsleep: Int = 0
  private var currentDay: String = ""
  def addSleep(day: String, minute: Int): Unit =
    currentGettingAsleep = minute
  def addWakeUp(day: String, minute: Int): Unit =
    days.update(day, (currentGettingAsleep, minute) +: days.getOrElse(day, Nil))

  def statistics: (Int, Int, Int, Int) =
    val minutes = Array.fill(60)(0)
    days.values.foreach:
      sleepsOfTheDay =>
        sleepsOfTheDay.foreach:
          case (gettingAsleep, gettingAwake) => (gettingAsleep until gettingAwake).foreach:
            index => minutes(index) = minutes(index) + 1

    val (bestMinuteFrequency, bestMinute) = minutes.zipWithIndex.maxBy(_._1)
    (id.toInt, minutes.sum, bestMinuteFrequency, bestMinute)