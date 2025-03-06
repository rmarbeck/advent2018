import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val initialRecipes = Vector(3,7)
    val target = inputLines.head

    val List(result1, result2) =
      List(Part1Stopper.initialize, Part2Stopper.initialize).map(_.apply(initialRecipes, target))
        .map:
          stopper => buildRecipes(initialRecipes, ElfPosition(0, initialRecipes(0)), ElfPosition(1, initialRecipes(1)))(using stopper)

    (s"$result1", s"$result2")

@tailrec
def buildRecipes(recipes: Vector[Int], firstElf: ElfPosition, secondElf: ElfPosition)(using stopper: Stopper): String =
  if (stopper.shouldStop)
    stopper.result(recipes)
  else
      val newFirstElf = firstElf.next(recipes)
      val newSecondElf = secondElf.next(recipes)
      val score = (newFirstElf.recipe + newSecondElf.recipe).toDigits
      stopper.push(score)
      buildRecipes(recipes.appendedAll(score), newFirstElf, newSecondElf)

import scala.collection.mutable
val toDigitsCache: mutable.Map[Int, Seq[Int]] = mutable.Map()

extension (value: Int)
  def toDigits: Seq[Int] = toDigitsCache.getOrElseUpdate(value, s"$value".map(_.asDigit))

case class ElfPosition(index: Int, recipe: Int):
  inline def next(currentRecipes: Vector[Int]): ElfPosition =
    val newIndex = (1 + index + recipe) % currentRecipes.size
    val newRecipe = currentRecipes(newIndex)
    ElfPosition(newIndex, newRecipe)

trait Stopper:
  def push(digits: Seq[Int]): Unit
  def shouldStop: Boolean
  def result(recipe: Vector[Int]): String

class Part1Stopper(initialRecipe: Vector[Int], target: String) extends Stopper:
  private val targetSize = target.toInt
  private var recipeSize: Int = initialRecipe.size

  override def shouldStop: Boolean = recipeSize >= targetSize + 10
  override def result(recipe: Vector[Int]): String = recipe.slice(targetSize, targetSize + 10).mkString
  override def push(digits: Seq[Int]): Unit = recipeSize += digits.length

object Part1Stopper:
  def initialize(initialRecipe: Vector[Int], target: String): Part1Stopper = new Part1Stopper(initialRecipe, target)

class Part2Stopper(initialRecipe: Vector[Int], target: String) extends Stopper:
  private val buffer: StringBuilder = StringBuilder(initialRecipe.mkString)
  private var recipeSize: Int = initialRecipe.size

  override def shouldStop: Boolean = buffer.startsWith(target)
  override def result(recipe: Vector[Int]): String =
    val shift = buffer.indexOf(target)
    s"${recipeSize - (target.length + 1 - shift)}"
  override def push(digits: Seq[Int]): Unit =
    recipeSize += digits.length
    buffer.append(digits.mkString)
    val overhead = buffer.length - target.length
    if (overhead > 0)
      buffer.delete(0, overhead - 1)

object Part2Stopper:
  def initialize(initialRecipe: Vector[Int], target: String): Part2Stopper = new Part2Stopper(initialRecipe, target)