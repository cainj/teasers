package hackerrank.algorithms.recursion

import scala.util.Try
import scala.language.postfixOps

/**
 * HackerRank CrosswordPuzzle
 *
 */
object CrosswordPuzzle {

  type Guess = List[(Char, Slot)]

  type Puzzle = Array[Array[Char]]

  type Words = List[String]

  type Path = List[Slot]

  /**
   * Find only starting slots
   *
   * @param crosswordPuzzle The crossword puzzle
   * @return
   */
  def startingSlots(crosswordPuzzle: Array[Array[Char]]): List[Slot] = {
    (for {
      i <- 0 until 10
      j <- 0 until 10
      slot = Slot(j, i)
      if slot.isStartSlot(crosswordPuzzle)
    } yield slot).toList
  }

  /**
   * Determine the directions of the slots
   *
   * @param slots The slots on the board
   * @return
   */
  def findDirection(slots: Path): String = {
    if (slots.length < 2) ""
    else {
      val slope = slots.takeRight(2) reduceLeft { (acc, next) =>
        Slot(acc.x - next.x, next.y)
      }
      if (Math.abs(slope.x) > 0) "right" else "down"
    }
  }

  def buildPath(slot: Slot, puzzle: Puzzle, result: Path): List[Path] =
    slot.availableSlots(puzzle, findDirection(slot :: result)) match {
      case Nil => List(result :+ slot)
      case x => x flatMap { buildPath(_, puzzle, result :+ slot) }
    }

  /**
    * Retuns as a stream b/c we want to quit once we've found one that works!
    *
    * @param words the words
    * @param paths the paths for the slots
    * @param result the guesses
    * @return
    */
  def guesses(words: Words, paths: List[Path], result: List[Guess]): Stream[List[Guess]] = {

    def makeGuess(words: Words, paths: List[Path], result: List[Guess]): List[Guess] = {
      (words, paths) match {
        case (Nil, Nil) => result
        case (_, Nil) => Nil
        case (Nil, _) => Nil
        case (w, p) =>
          val (l, r) = p span {
            w.head.length != _.length
          }
          if (r.nonEmpty) makeGuess(w.tail, l ++ r.tail, zip(w.head, r.head) :: result)
          else Nil
      }
    }

    for {
      combo <- words.permutations.toStream
    } yield makeGuess(combo, paths, Nil)
  }


  def solve(guesses: List[Guess], crosswordPuzzle: Array[Array[Char]], words: Words): Boolean = {
    guesses.foldLeft(List.empty[List[Slot]]) { (answers, next) =>
      for ((c, slot) <- next) yield { crosswordPuzzle(slot.y)(slot.x) = c }
      next.map { _._2 } :: answers
    } forall (x => check(x, crosswordPuzzle, words))
  }

  /**
   * Check to see if the guesses are correct
   *
   * @param guesses The guesses
   * @param puzzle The puzzle
   * @param words The words given
   * @return
   */
  private def check(guesses: List[Slot], puzzle: Puzzle, words: Words): Boolean = {
    val chars = guesses map { guess => puzzle(guess.y)(guess.x) }
    words.contains(new String(chars.toArray))
  }

  /**
   * Create a guess
   * @param word the word
   * @param path the path from the puzzle
   * @return
   */
  private def zip(word: String, path: Path): Guess = path.zipWithIndex.map { p => (word(p._2), p._1) }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val crosswordPuzzle = new Array[Array[Char]](10)
    for (i <- 0 until 10) { crosswordPuzzle(i) = sc.next.toCharArray }
    val words = sc.next().split(";") toList
    //given the crossword puzzle, find all available paths
    val paths = for {
      slot <- startingSlots(crosswordPuzzle)
      path <- buildPath(slot, crosswordPuzzle, Nil)
    } yield path

    val answer = guesses(words, paths, Nil) collectFirst {
      case guess if solve(guess, crosswordPuzzle, words) =>
        val puzzle = new Array[Array[Char]](10)
        crosswordPuzzle.copyToArray(puzzle)
        puzzle.map(new String(_)).reduce(_ + "\n" + _)
    }

    println(answer getOrElse (throw new IllegalStateException("Unsolvable puzzle")))
  }
  /**
   * Represents the slots on the crossword puzzle
   * @param x - x axis
   * @param y - y axis
   */
  case class Slot(x: Int, y: Int) {

    def DOWN: Slot = this.copy(x, y + 1)

    def RIGHT: Slot = this.copy(x + 1, y)

    def UP: Slot = this.copy(x, y - 1)

    def LEFT: Slot = this.copy(x - 1, y)

    /**
      * !!Hack!!!
      * @param puzzle
      * @return
      */
    def isStartSlot(puzzle: Puzzle): Boolean = {
      val direction = (look(UP, puzzle), look(DOWN, puzzle), look(LEFT, puzzle), look(RIGHT, puzzle)) match {
        case (true, true, false, false) => false
        case (true, false, false, false) => false
        case (false, false, true, true) => false
        case (false, true, false, false) => true
        case (true, true, false, true) => true
        case (false, true, true, false) => true
        case (false, false, false, true) => true
        case (false, true, true, true) => true
        case (true, false, false, true) => true
        case _ => false
      }
      direction && puzzle(y)(x) == '-'
    }

    private def look(slot: Slot, puzzle: Puzzle) = Try { puzzle(slot.y)(slot.x) == '-' } getOrElse false

    def availableSlots(puzzle: Puzzle, direction: String = ""): List[Slot] = {
      direction match {
        case "" =>
          val ways = List(DOWN, RIGHT, UP, LEFT) filter (look(_, puzzle))
          if (ways.length > 2)
            if (ways.diff(List(DOWN, RIGHT, LEFT)).isEmpty) List(DOWN) filter (look(_, puzzle))
            else List(RIGHT) filter (look(_, puzzle))
          else ways filterNot (UP==) filterNot (LEFT==)
        case "down" => List(DOWN) filter (look(_, puzzle))
        case "right" => List(RIGHT) filter (look(_, puzzle))
      }
    }
  }
}

