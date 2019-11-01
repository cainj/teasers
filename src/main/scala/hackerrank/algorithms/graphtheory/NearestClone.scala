package hackerrank.algorithms.graphtheory

import java.util

import scala.collection.mutable


object NearestClone {
  type Graph = Array[GraphNode]

  class GraphNode(val id: Int, val color: Long, val neighbors: mutable.HashSet[GraphNode] = mutable.HashSet.empty) {
    def addNeighbors(node: GraphNode): Boolean = neighbors.add(node)
  }

  def findStart(color: Long, graph: Graph): GraphNode = graph(color.toInt - 1)

  def findShortest(graphNodes: Int, graphFrom: Array[Int], graphTo: Array[Int], ids: Array[Long], `val`: Int): Int = {
    // create nodes with ids
    val graph = Array.tabulate(graphNodes) { i => new GraphNode(i + 1, ids(i)) }

    //add edges
    for (index <- graphTo.indices) {
      val to = graph(graphTo(index) - 1)
      val from = graph(graphFrom(index) - 1)
      to.addNeighbors(from)
      from.addNeighbors(to)
    }

    val start = findStart(`val`.toLong, graph)
    findShortestPathBFS(start, `val`.toLong, start.id)
  }

  private def findShortestPathBFS(node: GraphNode, color: Long, startId: Int): Int = {
    // put the node onto the queue

    //while the queue is not empty
    //dequeue check to see if the goal has been hit
    //if it has not put the neighbors from the visited node onto the queue
    //add current node to the visited set

    val queue = new util.ArrayDeque[List[GraphNode]]()
    val visited = mutable.HashSet.empty[GraphNode]

    queue.offer(List(node))

    while (!queue.isEmpty) {
      val path = queue.remove()
      val currentNode = path.last

      if (currentNode.color == color && currentNode.id != startId)
        return path.size - 1

      currentNode.neighbors.diff(visited).foreach { it =>
        queue.add(path.:+(it))
        visited.add(it)
      }
    }

    -1
  }

  def main(args: Array[String]): Unit = {
    val stdin = scala.io.StdIn

    val Array(graphNodes, graphEdges) = stdin.readLine().trim.split(" ").map(_.toInt)

    val graphFrom = Array.ofDim[Int](graphEdges)
    val graphTo = Array.ofDim[Int](graphEdges)

    for (i <- 0 until graphEdges) {
      val graphFromTo = stdin.readLine().trim.split(" ")

      graphFrom(i) = graphFromTo(0).toInt
      graphTo(i) = graphFromTo(1).toInt
    }

    val ids = stdin.readLine.split(" ").map(_.trim.toLong)
    val `val` = stdin.readLine.trim.toInt

    val ans = findShortest(graphNodes, graphFrom, graphTo, ids, `val`)

    println(ans)
  }
}
