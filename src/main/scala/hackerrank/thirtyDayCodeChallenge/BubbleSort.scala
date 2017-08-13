package hackerrank.thirtyDayCodeChallenge

object BubbleSort {

  def bubbleSort(array: Array[Int], swaps: Int): Int = {
    var numOfSwaps = 0

    for (i <- 0 until array.length - 1)
      if (array(i + 1) < array(i)) {
        val temp = array(i)
        array(i) = array(i + 1)
        array(i + 1) = temp
        numOfSwaps += 1
      }

    // Repeat until we don't have anymore swaps
    if (numOfSwaps == 0)
      swaps
    else
      bubbleSort(array, swaps + numOfSwaps)
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    val n = sc.nextInt();
    val a = new Array[Int](n);
    for (a_i <- 0 to n - 1) {
      a(a_i) = sc.nextInt();
    }
    println(s"Array is sorted in ${bubbleSort(a, 0)} swaps.")
    println(s"First Element: ${a.head}")
    println(s"Last Element: ${a.last}")

  }
}
