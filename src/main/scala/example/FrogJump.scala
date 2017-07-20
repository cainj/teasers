package example

/**
 *  Coding example by https://codility.com
 *
 * A small frog wants to get to the other side of the road. The frog is currently located at position X and wants to get to a position greater than or equal to Y. The small frog always jumps a fixed distance, D.
 *
 * Count the minimal number of jumps that the small frog must perform to reach its target.
 *
 * Write a function:
 *
 * object Solution { def solution(x: Int, y: Int, d: Int): Int }
 *
 * that, given three integers X, Y and D, returns the minimal number of jumps from position X to a position equal to or greater than Y.
 *
 * For example, given:
 *
 * X = 10
 * Y = 85
 * D = 30
 * the function should return 3, because the frog will be positioned as follows:
 *
 * after the first jump, at position 10 + 30 = 40
 * after the second jump, at position 10 + 30 + 30 = 70
 * after the third jump, at position 10 + 30 + 30 + 30 = 100
 * Assume that:
 *
 * X, Y and D are integers within the range [1..1,000,000,000];
 * X ≤ Y.
 * Complexity:
 *
 * expected worst-case time complexity is O(1);
 * expected worst-case space complexity is O(1).
 */
object FrogJump {

  def solution(x: Int, y: Int, d: Int): Int = {
    if (x + d >= y) 1
    else {
      val remainder = y % d
      val i = y / d
      if (remainder - x > 0) i + 1 else i
    }
  }

  def main(args: Array[String]): Unit = {
    //Should pass
    assert(solution(25, 75, 30) == 2)

    assert(solution(25, 85, 30) == 2)

    assert(solution(10, 85, 30) == 3)

    assert(solution(100, 100, 100) == 1)

    assert(solution(1, 95, 1) == 95)

  }

}
