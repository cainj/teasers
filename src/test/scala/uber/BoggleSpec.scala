package uber

import org.scalatest.{FlatSpec, Matchers}

class BoggleSpec extends FlatSpec with Matchers {
  "The Boggle object" should "find words" in {
    Boggle.solve(
      Array("GEEKS", "FOR", "QUIZ", "GO"), Array(Array('G', 'I', 'Z'), Array('U', 'E', 'K'), Array('Q', 'S', 'E'))) shouldEqual (Vector("GEEKS", "QUIZ"))
  }
}
