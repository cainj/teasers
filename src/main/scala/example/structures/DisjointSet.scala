package example.structures

/**
 *
 */
class DisjointSet(n: Int) {
  var i = -1
  private val id = Array.fill(10)({
    i += 1
    i
  })

  private def root(i: Int): Int = {
    var x = i
    while (x != id(x)) x = id(x)
    x
  }

  def union(p: Int, q: Int) = {
    val i = root(p)
    val j = root(q)
    id(i) = j
  }

  def connected(p: Int, q: Int) = root(p) == root(q)
}
