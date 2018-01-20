package coursera.algos2.week1

import scala.io.Source

object DFS {

  type Edges = Iterator[String]
  type Graph = Array[List[Int]]

  def main(args: Array[String]) {
    val fileWithNumbers = "SCC.txt"
    val edges = Source.fromResource(fileWithNumbers).getLines()
    val graph: Graph = new Array(875714)
    updateGraph(edges, graph)
    println(graph(0))
  }

  private[this] def updateGraph(edges: Edges, graph: Graph): Unit = {
    for {
      edge <- edges.toList
      (u, v) = {
        val e = edge.trim.split(" ")
        (e(0), e(1))
      }
    } yield graph(u.toInt - 1) match {
      case null => graph(u.toInt - 1) = List(v.toInt)
      case head :: Nil => graph(u.toInt - 1) = List(head).:+(v.toInt)
      case list => graph(u.toInt - 1) = list.:+(v.toInt)
    }
  }
}
