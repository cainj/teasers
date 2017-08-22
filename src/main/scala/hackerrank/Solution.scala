package hackerrank

import java.util.Scanner

object Solution {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    val x = sc.nextLine().split(" ") map { _.toInt }
    val y = sc.nextLine().split(" ") map { _.toInt }

    val rDay = x(0)
    val rMonth = x(1)
    val rYear = x(2)

    val dDay = y(0)
    val dMonth = y(1)
    val dYear = y(2)

    val due =
      if (rYear < dYear) 0
      else if (rYear > dYear) 10000
      else if (rMonth > dMonth) 500 * (rMonth - dMonth)
      else if (rDay > dDay) 15 * (rDay - dDay)
      else 0
    println(due)
  }
}
