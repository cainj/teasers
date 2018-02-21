package interview_cake

import scala.util.Random

/**
 * Write a function for doing an in-place shuffle of a list.
 * The shuffle must be "uniform," meaning each item in the original list must have the same
 * probability of ending up in each spot in the final list.
 * Assume that you have a function get_random(floor, ceiling) for getting a random integer
 * that is >= floor and <= ceiling.
 */
object InPlaceShuffle {

  def main(args: Array[String]): Unit = {
    println(shuffle(Array(0, 1, 2, 3, 4, 5, 6)).toList)
  }

  def shuffle[A](array: Array[A]) = {
    for {
      pos <- array.indices
      r = getRandom(pos, array.length - 1)
    } yield swap(r, pos, array)
    array
  }

  def getRandom(floor: Int, ceiling: Int) = floor + Random.nextInt((ceiling - floor) + 1)

  def swap[A](x: Int, y: Int, arr: Array[A]): Unit = {
    val hold = arr(x)
    arr(x) = arr(y)
    arr(y) = hold
  }

}
