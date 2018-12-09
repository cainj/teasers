package codility

import org.scalatest.{FlatSpec, Matchers}

/**
 *
 */
class PermCheckSpec extends FlatSpec with Matchers {
  "The PermCheck object" should "find words" in {
    PermCheck.solution(Array(4, 3, 1, 2)) shouldEqual 1
    PermCheck.solution(Array(4, 3, 1)) shouldEqual 0
    PermCheck.solution(Array(4, 3, 2)) shouldEqual 0
    PermCheck.solution(Array(3, 2, 1)) shouldEqual 1
    PermCheck.solution(Array(5, 3, 2, 1)) shouldEqual 0
    PermCheck.solution(Array(2)) shouldEqual 0
    PermCheck.solution(Array(1)) shouldEqual 1
    PermCheck.solution(Array(0)) shouldEqual 0
  }
}
