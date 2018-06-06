package ibotta

object RangeReduce {

  type Range = (Int, Int)
  type Ranges = List[(Int, Int)]

  object RangerOrdering extends Ordering[(Int, Int)] {
    override def compare(x: (Int, Int), y: (Int, Int)): Int = x._1 compare y._1
  }

  def main(args: Array[String]) {
    val parens = List((8, 9), (12, 13), (1, 5), (4, 7))
    println(rangeReduceWithFold(parens))
  }

  def rangeReduce(range: Ranges): Ranges = {
    val sorted = range.sorted
    var stack = List.empty[Range]
    for (i <- sorted.indices) {
      val compare = sorted(i)
      if (stack.nonEmpty) {
        val head = stack.head
        if (intersect_?(compare, head))
          stack = doReduce(compare, head) :: stack.tail
        else
          stack = compare :: stack
      } else {
        stack = compare :: stack
      }
    }
    stack
  }

  def rangeReduceWithFold(ranges: Ranges): Ranges = {
    val sorted = ranges.sorted
    sorted.foldLeft(List.empty[Range]) {
      (stack, range) =>
        stack match {
          case head :: tail if intersect_?(range, head) =>
            doReduce(range, head) :: tail
          case _ => range :: stack
        }

    }
  }

  def doReduce(l: Range, r: Range): Range = {
    val x = l._1.min(r._1)
    val y = l._2.max(r._2)
    (x, y)
  }

  private def intersect_?(l: Range, r: Range): Boolean = {
    if (l._1 >= r._1 && l._1 <= r._2 || l._2 >= r._1 && l._2 <= r._2) true
    else false
  }
}
