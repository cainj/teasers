package hackerrank.algorithms.graphtheory


/**
  * https://www.hackerrank.com/challenges/journey-to-the-moon/problem
  */
object JourneyToMoon {

  type Astronauts = Array[Array[Int]]
  type Group = List[Int]

  def createGraph(astronauts: Astronauts, n: Int) = {
    var graph = (0 until n).foldLeft(Map.empty[Int, List[Int]]) {
      (acc, next) => acc + (next -> List.empty[Int])
    }
    for {
      i <- astronauts.indices
    } yield {
      val ast = astronauts(i)
      var edges = graph.getOrElse(ast(0), List.empty)
      graph = graph + (ast(0) -> (ast(1) :: edges))
      edges = graph.getOrElse(ast(1), List.empty)
      graph = graph + (ast(1) -> (ast(0) :: edges))
    }
    graph
  }

  private[this] def createGroups(astronaut: Int, astronauts: Map[Int, List[Int]], visited: Array[Boolean]): Long = {
    visited(astronaut) = true
    val edges = astronauts(astronaut)
    var i = 0
    var c = 1L
    while (i < edges.length) {
      val edge = edges(i)
      if (!visited(edge)) c += createGroups(edge, astronauts, visited)
      i += 1
    }
    c
  }

  def journeyToMoon(n: Int, astronaut: Astronauts): Long = {
    // Complete this function
    //create a graph to represent the connections between the astronauts
    val graph = createGraph(astronaut, n)
    val visited = new Array[Boolean](n)
    val groups = for {
      i <- visited.indices
      if !visited(i)
    } yield createGroups(i, graph, visited)

    solve(groups)
  }

  private def solve(groups: Seq[Long]) = {
    var result = 0L
    var sum = 0L

    for (size <- groups) {
      result += sum * size
      sum += size
    }
    result
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    val p = sc.nextInt()
    val astronaut = Array.ofDim[Int](p, 2)
    for {
      i <- 0 until p
      j <- 0 until 2
    } {
      astronaut(i)(j) = sc.nextInt()
    }
    val result = journeyToMoon(n, astronaut)
    println(result)
  }
}
