package hackerrank.algorithms.graphtheory

/**
 * https://www.hackerrank.com/challenges/bfsshortreach/problem
 */
object BreadFirstSearch {

  type Graph = Array[Array[Int]]

  def bfs(n: Int, m: Int, edges: Array[Array[Int]], s: Int): Array[Int] = {
    // Complete this function
    val graph = createGraph(edges, n)

    def dijkstra(graph: Graph, start: Int): Array[Int] = {
      val dist = Array.fill(n)(Int.MaxValue)
      dist(s - 1) = 0
      var i = 0
      val visited = new Array[Boolean](n)
      while (i < n - 1) {
        val u = minDistance(dist, visited, n)
        visited(u) = true
        for {
          v <- 0 until n
          if !visited(v) && graph(u)(v) != 0 && dist(u) != Int.MaxValue && dist(u) + graph(u)(v) < dist(v)
        } yield dist(v) = dist(u) + graph(u)(v)
        i += 1
      }
      dist filter (_ != 0) map { d => if (d == Int.MaxValue) -1 else d * 6 }

    }
    dijkstra(graph, s)
  }

  private[this] def minDistance(dist: Array[Int], sptSet: Array[Boolean], n: Int): Int = {
    // Initialize min value// Initialize min value

    var min: Int = Integer.MAX_VALUE
    var minIndex: Int = -1

    var v: Int = 0
    while (v < n) {
      if (!sptSet(v) && dist(v) <= min) {
        min = dist(v)
        minIndex = v
      }
      v += 1
    }
    minIndex
  }

  def createGraph(edges: Array[Array[Int]], n: Int) = {
    val graph = Array.ofDim[Int](n, n)
    for (edge <- edges) {
      graph(edge(0) - 1)(edge(1) - 1) = 1
      graph(edge(1) - 1)(edge(0) - 1) = 1
    }
    graph
  }
  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val q = sc.nextInt()
    var a0 = 0
    while (a0 < q) {
      val n = sc.nextInt()
      val m = sc.nextInt()
      val edges = Array.ofDim[Int](m, 2)
      for {
        i <- 0 until m
        j <- 0 until 2
      } yield edges(i)(j) = sc.nextInt()
      val s = sc.nextInt()
      val result = bfs(n, m, edges, s)
      println(result.mkString(" "))
      a0 += 1
    }
  }
}
