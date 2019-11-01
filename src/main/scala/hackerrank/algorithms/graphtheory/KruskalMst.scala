package hackerrank.algorithms.graphtheory

import example.structures.DisjointSet

import scala.collection.mutable
import scala.io.StdIn

case class Edge(from: Int, to: Int, weight: Int)

object KruskalMst {

  /*
   * Complete the 'kruskals' function below.
   *
   * The function is expected to return an INTEGER.
   * The function accepts WEIGHTED_INTEGER_GRAPH g as parameter.
   */

  /*
   * For the weighted graph, <name>:
   *
   * 1. The number of nodes is <name>Nodes.
   * 2. The number of edges is <name>Edges.
   * 3. An edge exists between <name>From[i] and <name>To[i]. The weight of the edge is <name>Weight[i].
   *
   */

  def kruskals(gNodes: Int, gFrom: Array[Int], gTo: Array[Int], gWeight: Array[Int]): Int = {
    // Write your code here
    var edges = mutable.MutableList.empty[Edge]
    for (index <- gFrom.indices) {
      edges += Edge(gFrom(index), gTo(index), gWeight(index))
    }

    //sort the edges
    edges = edges.sortBy(_.weight)

    val disjointSet = new DisjointSet[Int]()
    for (node <- 1 to gNodes) {
      disjointSet.makeSet(node)
    }

    val result = mutable.MutableList.empty[Edge]

    for (edge <- edges) {
      if (isNotCycle(edge, disjointSet)) {
        val root1 = disjointSet.findSet(edge.to)
        val root2 = disjointSet.findSet(edge.from)

        if (root1 != root2) {
          result += edge
          disjointSet.union(edge.to, edge.from)
        }
      }
    }

    result.foldLeft(0) { (accum, next) =>
      accum + next.weight
    }
  }

  def isNotCycle(edge: Edge, disjointSet: DisjointSet[Int]): Boolean =
    disjointSet.findSet(edge.to) != disjointSet.findSet(edge.from)

}

object Solution {
  def main(args: Array[String]) {

    val Array(gNodes, gEdges) = StdIn.readLine().replaceAll("\\s+$", "").split(" ").map(_.toInt)

    val gFrom = Array.ofDim[Int](gEdges)
    val gTo = Array.ofDim[Int](gEdges)
    val gWeight = Array.ofDim[Int](gEdges)

    for (i <- 0 until gEdges) {
      val gFromToWeight = StdIn.readLine().replaceAll("\\s+$", "").split(" ")

      gFrom(i) = gFromToWeight(0).toInt
      gTo(i) = gFromToWeight(1).toInt
      gWeight(i) = gFromToWeight(2).toInt
    }

    val res = KruskalMst.kruskals(gNodes, gFrom, gTo, gWeight)

    // Write your code here.
    println(res)
  }
}
