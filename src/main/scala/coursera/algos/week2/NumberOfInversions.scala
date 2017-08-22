package coursera.algos.week2

import scala.io.Source

object NumberOfInversions {
  def main(args: Array[String]) {
    val fileWithNumbers = "IntegerArray.txt"
    val start = System.currentTimeMillis()
    val inversions: BigInt = numberOfInversions(Source.fromResource(fileWithNumbers).getLines().map(Integer.parseInt).toArray, 100000)
    val end = System.currentTimeMillis()
    println("Total time: " + (end - start))
    println(inversions)
  }

  /* This method sorts the input array and returns the
       number of inversions in the array */ /* This method sorts the input array and returns the
       number of inversions in the array */
  def numberOfInversions(arr: Array[Int], array_size: Int): Long = {
    val temp = new Array[Int](array_size)
    _mergeSort(arr, temp, 0, array_size - 1)
  }

  /* An auxiliary recursive method that sorts the input array and
    returns the number of inversions in the array. */
  def _mergeSort(arr: Array[Int], temp: Array[Int], left: Int, right: Int): Long = {
    var mid = 0
    var inv_count = 0L
    if (right > left) {
      /* Divide the array into two parts and call _mergeSortAndCountInv()
                for each of the parts */
      mid = (right + left) / 2
      /* Inversion count will be sum of inversions in left-part, right-part
                and number of inversions in merging */
      inv_count = _mergeSort(arr, temp, left, mid)
      inv_count += _mergeSort(arr, temp, mid + 1, right)
      /*Merge the two parts*/
      inv_count += merge(arr, temp, left, mid + 1, right)
    }
    inv_count
  }

  /* This method merges two sorted arrays and returns inversion count in
     the arrays.*/
  def merge(arr: Array[Int], temp: Array[Int], left: Int, mid: Int, right: Int): Long = {
    var inv_count = 0L
    var i = left /* i is index for left subarray*/
    var j = mid /* j is index for right subarray*/
    var k = left /* k is index for resultant merged subarray*/
    while ((i <= mid - 1) && (j <= right)) {
      if (arr(i) <= arr(j)) {
        temp(k) = arr(i)
        k += 1
        i += 1

      } else {
        temp(k) = arr(j)
        k += 1
        j += 1

        /*this is tricky -- see above explanation/diagram for merge()*/
        inv_count = inv_count + (mid - i)
      }
    }
    /* Copy the remaining elements of left subarray
           (if there are any) to temp*/
    while (i <= mid - 1) {
      temp(k) = arr(i)
      i += 1
      k += 1

    }
    /* Copy the remaining elements of right subarray
           (if there are any) to temp*/
    while (j <= right) {
      temp(k) = arr(j)
      k += 1
      j += 1

    }
    /*Copy back the merged elements to original array*/
    i = left
    while (i <= right) {
      arr(i) = temp(i)
      i += 1
    }
    inv_count
  }
}
