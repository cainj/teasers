package hackerrank.datastructures.arrays

object ArrayDS {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    val n = sc.nextInt();
    val arr = new Array[Int](n);
    for (arr_i <- 0 to n - 1) {
      arr(arr_i) = sc.nextInt();
    }

    for (i <- arr.length - 1 to 0 by -1) {
      print(s"${arr(i)} ")
    }
  }
}
