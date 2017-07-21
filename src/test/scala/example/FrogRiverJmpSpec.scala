package example

import org.scalatest.{ Matchers, FlatSpec }

/**
 *
 */
class FrogRiverJmpSpec extends FlatSpec with Matchers {
  "The FrogRiverJmp object" should "find jumps" in {
    FrogRiverJmp.solutionF(5, Array(1, 3, 1, 4, 2, 3, 5, 4)) shouldEqual 6
    FrogRiverJmp.solutionF(6, Array(1, 3, 1, 4, 2, 3, 5, 4)) shouldEqual -1
    FrogRiverJmp.solutionF(3, Array(1, 3, 1, 3, 2, 1, 3)) shouldEqual 4
  }

  "The FrogRiverJmp object" should "find jumps iteratively" in {
    FrogRiverJmp.solution(5, Array(1, 3, 1, 4, 2, 3, 5, 4)) shouldEqual 6
    FrogRiverJmp.solution(6, Array(1, 3, 1, 4, 2, 3, 5, 4)) shouldEqual -1
    FrogRiverJmp.solution(3, Array(1, 3, 1, 3, 2, 1, 3)) shouldEqual 4
  }
}
