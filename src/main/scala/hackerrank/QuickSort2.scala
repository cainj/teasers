package hackerrank

/**
 *
 */
object QuickSort2 {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    val a = new Array[Int](n)
    for (i <- 0 to n - 1) {
      a(i) = sc.nextInt()
    }

    quicksort(a, 0, a.length)
  }

  def partition(array: Array[Int], left: Int, right: Int): Int = {
    val pivot = array(left)
    var smallerElements = List.empty[Int]
    var largerElements = List.empty[Int]

    for (i <- left + 1 until right) {
      if (array(i) < pivot) {
        smallerElements = smallerElements :+ (array(i))
      } else {
        largerElements = largerElements :+ (array(i))
      }
    }

    for (i <- 0 until smallerElements.length) {
      array(left + i) = smallerElements(i)
    }

    array(left + smallerElements.length) = pivot

    for (i <- 0 until largerElements.length) {
      array(left + smallerElements.size + 1 + i) = largerElements(i)
    }

    left + smallerElements.size
  }

  def quicksort(array: Array[Int], left: Int, right: Int) {
    if (right - left >= 2) {
      val pivot: Int = partition(array, left, right)
      quicksort(array, left, pivot)
      quicksort(array, (pivot + 1), right)
      println(array.toList.slice(left, right) mkString (" "))
    }
  }
}

//12 11 13 5 6