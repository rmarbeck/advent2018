import scala.collection.*

final class NegativeSupportedBitSet extends mutable.Set[Int] {
  private val positives = mutable.BitSet()
  private val negatives = mutable.BitSet()

  private def selected(elem: Int): mutable.BitSet =
    elem match
      case value if value >= 0 => positives
      case _ => negatives

  override def clear(): Unit =
    positives.clear()
    negatives.clear()

  override def addOne(elem: Int): NegativeSupportedBitSet.this.type =
    selected(elem).addOne(elem.abs)
    this

  override def contains(elem: Int): Boolean = selected(elem).contains(elem.abs)

  override def iterator: Iterator[Int] = negatives.iterator.map(- _) ++ positives.iterator

  override def subtractOne(elem: Int): NegativeSupportedBitSet.this.type =
    selected(elem).contains(elem.abs)
    this
}
