package hackerrank.algorithms.search

object RoadLibraries {

  type Cities = Array[Array[Long]]

  def roadsAndLibraries(numOfCities: Long, libraries: Long, roads: Long, cities: Cities): Long = {

    if (roads >= libraries || (cities.length == 0)) libraries * numOfCities

    else {
      val graph = Array.ofDim[Long](numOfCities.toInt + 1, numOfCities.toInt + 1)

      for (i <- cities.indices) {
        val v = cities(i)(0)
        val w = cities(i)(1)
        graph(v.toInt)(w.toInt) = 1
      }

      val visited = new Array[Boolean](numOfCities.toInt + 1)
      var components = 0

      for (i <- 1 until graph.length) {
        // means the node is still not visited
        if (!visited(i)) {
          search(i, graph, visited)
          components += 1
        }
      }
      roads * (numOfCities - components) + components * libraries
    }
  }

  private[this] def search(city: Int, graph: Cities, visited: Array[Boolean]) {
    visited(city) = true
    for {
      i <- 1 until graph(city).length
      if graph(city)(i) == 1 && !visited(i)
    } yield search(i, graph, visited)
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    var q = sc.nextInt()
    var a0 = 0
    while (a0 < q) {
      val n = sc.nextInt()
      val m = sc.nextInt()
      val c_lib = sc.nextInt()
      val c_road = sc.nextInt()
      val cities = Array.ofDim[Long](m, 2)

      for (i <- 0 until m; j <- 0 to 1) {
        cities(i)(j) = sc.nextInt()
      }
      val result = roadsAndLibraries(n, c_lib, c_road, cities)
      println("***" + result)
      a0 += 1
    }
  }
}
