package codility

import org.scalatest.{FlatSpec, Matchers}

/**
 *
 */
class MaxCountersSpec extends FlatSpec with Matchers {
  "The MaxCounter object" should "find words" in {
    MaxCounters.solution(5, Array(3, 4, 4, 6, 1, 4, 4)) shouldEqual Array(3, 2, 2, 4, 2)
    MaxCounters.solution(4, Array(5, 5, 5, 5, 5, 5, 5)) shouldEqual Array(0, 0, 0, 0)
    MaxCounters.solution(5, Array(6, 6, 6, 6, 6, 6, 6)) shouldEqual Array(0, 0, 0, 0, 0)
  }
}

