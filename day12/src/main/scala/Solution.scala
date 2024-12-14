val part2Generations = 50_000_000_000L

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val pots = inputLines.head match
      case s"initial state: ${pots}" =>
          Pots(pots.map(_.toBoolean).toVector)

    given Seq[Rule] = inputLines.tail.collect:
      case s"${pattern} => ${pot}" =>
        Rule(Pattern.fromString(pattern), pot.head.toBoolean)

    val potsAfter20 =
      (1 to 20).foldLeft(pots):
        case (current, _) => current.next

    val significantNbOfGen = 250

    val (potsAfter2000, List(firstSignificantValue, secondSignificantValue)) =
      (1 to significantNbOfGen).foldLeft((pots, Nil): (Pots, List[Int])):
        case ((current, significant), index) if (index % (significantNbOfGen/2) == 0) =>
          val next = current.next
          (next, next.sum :: significant)
        case ((current, significant), index) => (current.next, significant)

    val factor = (firstSignificantValue - secondSignificantValue) / (significantNbOfGen / 2)
    val diff = (significantNbOfGen * factor) - potsAfter2000.sum

    val result1 = s"${potsAfter20.sum}"
    val result2 = s"${part2Generations * factor - diff}"

    (s"$result1", s"$result2")


case class Pots(plants: Vector[Boolean], firstIndex: Int = 0):
  lazy val sum: Int =
    plants.foldLeft((0, firstIndex)):
      case ((acc, index), true) => (acc + index, index + 1)
      case ((acc, index), _) => (acc, index + 1)
    ._1

  def next(using rules: Seq[Rule]): Pots =
    val newValues = (Vector(false, false, false) ++ plants ++ Vector(false, false, false)).sliding(5,1).map:
      case current =>
        rules.find(_.pattern == Pattern(current)).exists(_.next)
    .toVector

    val drift = newValues.head match
      case false => 1
      case true => 0
    Pots(newValues.drop(1 - drift), firstIndex - drift)


case class Pattern(mask: Vector[Boolean])

extension (char: Char)
  def toBoolean: Boolean =
    char match
      case '.' => false
      case '#' => true
      case _ => throw Exception("Not supported")

object Pattern:
  def fromString(input: String): Pattern = Pattern(input.map(_.toBoolean).toVector)

case class Rule(pattern: Pattern, next: Boolean)

