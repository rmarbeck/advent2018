import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC: Logger = Logger("aoc")
val loggerAOCPart1: Logger = Logger("aoc.part1")
val loggerAOCPart2: Logger = Logger("aoc.part2")

@main def hello(): Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day14")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : $score1")
    println(s"2 : $score2")
    println(s"----------------")
  println("Done")

object Solver:
  private def runOn(inputLines: Seq[String]): (String, String) =

    val (result1, result2) = Solution.run(inputLines)

    (s"$result1", s"$result2")

  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromResource(fileName)
    val lines = bufferedSource.getLines().toSeq
    lines match
      case Nil => ("", "")
      case _ => runOn(lines)
end Solver

