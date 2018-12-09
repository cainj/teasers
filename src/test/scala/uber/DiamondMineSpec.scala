package uber

import org.scalatest.{FlatSpec, Matchers}

class DiamondMineSpec extends FlatSpec with Matchers {
  "The DiamondMine object" should "find max diamonds" in {
    DiamondMine.collectMax(Array(Array(0, 1, 1), Array(1, 0, -1), Array(1, 1, 1))) shouldEqual 4
    DiamondMine.collectMax(Array(Array(0, 1, -1), Array(1, 0, -1), Array(1, -1, 0))) shouldEqual 0
  }
}
