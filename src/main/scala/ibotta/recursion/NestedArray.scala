package ibotta.recursion

/**
 *
 */
object NestedArray {

  def main(args: Array[String]) {
    val arr = List[Any](1, 5, 2, List(9, 4, List(7), 8), 3)
    println(sumOfArraysPatternMatch(arr))
  }

  def sumOfArrays(arr: List[Any]): Int = {
    var sum = 0
    if (arr.isEmpty) sum
    else {
      for (i <- arr.indices) yield {
        if (arr(i).isInstanceOf[List[Any]])
          sum = sumOfArraysPatternMatch(arr(i).asInstanceOf[List[Any]]) + sum
        else
          sum += arr(i).asInstanceOf[Int]
      }
      sum
    }
  }

  def sumOfArraysPatternMatch(arr: List[Any], sum: Int = 0): Int = {
    arr match {
      case Nil => sum
      case head :: tail if arr.head.isInstanceOf[List[Any]] =>
        sumOfArraysPatternMatch(head.asInstanceOf[List[Any]]) + sumOfArraysPatternMatch(tail, sum)
      case _ => sumOfArraysPatternMatch(arr.tail, sum + arr.head.asInstanceOf[Int])
    }
  }
}

