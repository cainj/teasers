package example.sorting

object NumberOfInversions {

  def numberOfInversions(ints: List[Int]): Int = {
    var numberOfInversions = 0

    def mergeSort(xs: List[Int]): List[Int] = {
      def sort(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (Nil, y) => y
        case (x, Nil) => x
        case (x, y) =>
          if (x.head < y.head)
            x.head :: sort(x.tail, y)
          else {
            numberOfInversions = numberOfInversions + x.length
            y.head :: sort(x, y.tail)
          }

      }

      val n = xs.length / 2
      if (n < 1) xs
      else {
        val (l, r) = xs.splitAt(n)
        sort(mergeSort(l), mergeSort(r))
      }
    }
    mergeSort(ints)
    numberOfInversions
  }

  def main(args: Array[String]) {
    println(numberOfInversions(List(1, 3, 5, 2, 4, 6)))
  }
}
