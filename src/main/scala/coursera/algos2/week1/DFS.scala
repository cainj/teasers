package coursera.algos2.week1

import scala.io.Source

object DFS {

  type Edges = Iterator[String]
  type Graph = Array[List[Int]]
  val Size = 6
  var stack = List.empty[Int]
  var explored = Set.empty[Int]

  def main(args: Array[String]) {
    val fileWithNumbers = "SCC.txt"
    val edges = Source.fromResource(fileWithNumbers).getLines()
    val graph: Graph = new Array(875714)
    //updateGraph(edges, graph)
    println(graph(0))
  }

  private[this] def depthFirstSearch(v: Int, graph: Graph) {
    if (graph(v) != null) {
      val x = graph(v).iterator
      explored = explored + v
      while (x.hasNext) {
        val n = x.next()
        if (!explored(n)) {
          depthFirstSearch(n, graph)
          println(stack)
        }
      }
    }
    stack = v :: stack
  }

  private[this] def createGraph(edges: Edges): Graph = {
    val graph: Graph = new Array(Size)
    for {
      edge <- edges.toList
      (u, v) = {
        val e = edge.trim.split(" ")
        (e(0), e(1))
      }
    } yield graph(u.toInt) match {
      case null => graph(u.toInt) = List(v.toInt)
      case head :: Nil => graph(u.toInt) = List(head).+:(v.toInt)
      case list => graph(u.toInt) = list.+:(v.toInt)
    }
    graph
  }

  private[this] def transpose(graph: Graph): Graph = {
    graph.indices.foldLeft(new Array[List[Int]](Size)) { (acc, next) =>
      val list = graph(next)
      if (list != null) {
        list.foreach { i =>
          acc(i) match {
            case null => acc(i) = List(next)
            case head :: Nil => acc(i) = List(head).:+(next)
            case ints => acc(i) = ints.:+(next)
          }
        }
      }
      acc
    }
  }
}
