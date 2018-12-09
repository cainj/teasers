package uber

import org.scalatest.{FlatSpec, Matchers}

class BalancerSpec extends FlatSpec with Matchers {
  "The Balancer object" should "find words" in {
    Balancer.balancedOrNot(Array("<>", "<>><"), Array(2, 1)).toList shouldEqual List(1, 0)
  }
}
