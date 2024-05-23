object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val rules = inputLines.collect:
      case s"Step $requisite must be finished before step $step can begin." => Rule(step, requisite)

    val steps = rules.map(rule => rule.step -> rule.preRequisite).groupMapReduce(_._1)((name, preRequisite) => Step(name, List(preRequisite)))(Step.merge)
    val withoutPreRequisite = steps.values.flatMap(_.preRequisites).toSet.filterNot(steps.contains)

    val newSteps = steps ++ withoutPreRequisite.map(name => name -> Step(name, Nil))
    val preparedSteps = newSteps.map((key, step) => key -> step.preRequisites)

    val resultPart1 = orderSteps(preparedSteps)

    val (maxWorkers, delay) = inputLines.size match
      case 7 => (2, 0)
      case _ => (5, 60)

    val resultPart2 = orderStepsPart2(preparedSteps, workingWorkers = Nil, maxWorkers = maxWorkers, elapsed = 0, delay = delay)._2

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

def orderSteps(steps: Map[String, List[String]], current: StringBuilder = StringBuilder()): String =
  steps.isEmpty match
    case true => current.toString
    case false =>
      val available = steps.keys
      val found =
        available.filter:
          case step if steps.contains(step) => steps(step).map(_.head).forall(current.contains)
          case step => true
        .toList.min
      orderSteps(steps - found, current.append(found))

def orderStepsPart2(steps: Map[String, List[String]], current: StringBuilder = StringBuilder(), workingWorkers: List[Worker], maxWorkers: Int, elapsed: Int, delay: Int = 60): (String, Int) =
  def timeToCompute(str: String): Int = delay + str.head.toInt - 64
  def populateWorkers(usingSteps: Map[String, List[String]], usingCurrent: StringBuilder, currentWorkers: List[Worker], previousWorkerElapsed: Int): List[Worker] =
    val available = usingSteps.keys
    val toAllocate =
      available.filter:
        case step if usingSteps.contains(step) => usingSteps(step).map(_.head).forall(usingCurrent.contains)
        case step => true
      .toList.sorted.filterNot(currentWorkers.map(_.workingOn).contains).take(maxWorkers - currentWorkers.size)
    toAllocate.map(step => Worker(step, timeToCompute(step))) ::: currentWorkers.map(_.removeElapsed(previousWorkerElapsed))

  steps.isEmpty match
    case true => (current.toString, elapsed + workingWorkers.map(_.remainingTime).maxOption.getOrElse(0))
    case false =>
      workingWorkers match
        case Nil =>
          val newWorkers = populateWorkers(steps, current, Nil, 0)
          orderStepsPart2(steps, current, newWorkers, maxWorkers, elapsed, delay)
        case workers =>
          val shortestWork = workers.map(_.remainingTime).min
          val firstEndingWorkers = workers.filter(_.remainingTime == shortestWork)
          val finishedSteps = firstEndingWorkers.map(_.workingOn).sorted
          val futureSteps = steps -- finishedSteps
          val futureCurrent = current.append(finishedSteps.mkString)
          val newWorkers = populateWorkers(futureSteps, futureCurrent, workers diff firstEndingWorkers, shortestWork)
          orderStepsPart2(futureSteps, futureCurrent, newWorkers, maxWorkers, elapsed+shortestWork, delay)

case class Worker(workingOn: String, remainingTime: Int):
  def removeElapsed(time: Int): Worker = this.copy(remainingTime = remainingTime - time)

case class Rule(step: String, preRequisite: String)

case class Step(name: String, preRequisites: List[String]):
  def merge(other: Step): Step = this.copy(preRequisites = this.preRequisites ::: other.preRequisites)

object Step:
  def merge(one: Step, other: Step): Step = one.merge(other)