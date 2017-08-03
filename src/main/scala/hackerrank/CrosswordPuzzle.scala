package hackerrank

import hackerrank.CrosswordPuzzle.Puzzle

import scala.util.Try

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
   * Find all the available slots in the puzzle
   *
   * @param crosswordPuzzle The crossword puzzle
   * @return
   */
  def findSlots(crosswordPuzzle: Array[Array[Char]]): Set[Slot] = {
    (for { i <- 0 until 10; j <- 0 until 10; if crosswordPuzzle(i)(j) == '-' } yield Slot(j, i)).toSet
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
      val slope = slots.takeRight(2) reduceLeft { (acc, next) => Slot(acc.x - next.x, next.y) }
      if (Math.abs(slope.x) > 0) "right" else "down"
    }
  }

  def findPaths(slots: Path, puzzle: Puzzle, result: Path): List[Path] = slots match {
    case Nil => List(result)
    case _ =>
      slots.flatMap {
        s => findPaths(s.availableSlots(puzzle, findDirection(result)), puzzle, result :+ s)
      }

  }

  /**
   * Remove the partial paths
   *
   * @param paths
   * @param result
   * @return
   */
  def removePartialPaths(paths: List[Path], result: List[Path]): List[Path] = {
    paths match {
      case Nil => result
      case head :: tail =>
        tail find (head.intersect(_) == head) match {
          case Some(_) => removePartialPaths(tail, result)
          case None => removePartialPaths(tail, head :: result)
        }
    }
  }

  def guesses(words: Words, paths: List[Path], result: List[Guess]): Set[List[Guess]] = {

    def makeGuess(words: Words, paths: List[Path], result: List[Guess]): List[Guess] =
      (words, paths) match {
        case (Nil, Nil) => result
        case (_, Nil) => Nil
        case (Nil, _) => Nil
        case (w, p) =>
          val (l, r) = p span { w.head.length != _.length }
          if (r.nonEmpty) makeGuess(w.tail, l ++ r.tail, zip(w.head, r.head) :: result)
          else Nil
      }
    (for {
      combo <- words.permutations.toList
    } yield makeGuess(combo, paths, Nil)).toSet
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
    val sizes = words.map(_.length)
    //given the crossword puzzle, find all available paths
    val paths = for {
      slot <- findSlots(crosswordPuzzle)
      path <- findPaths(List(slot), crosswordPuzzle, Nil)
      if sizes.contains(path.size)
    } yield path

    val answer = for {
      guess <- guesses(words, removePartialPaths(paths.toList.sortWith(_.length < _.length), Nil), Nil)
      if solve(guess, crosswordPuzzle, words)
    } yield {
      val puzzle = new Array[Array[Char]](10)
      crosswordPuzzle.copyToArray(puzzle)
      puzzle.map(new String(_)).reduce(_ + "\n" + _)
    }
    println(answer.headOption getOrElse (throw new IllegalStateException("Unsolvable puzzle")))
  }
  /**
   * Represents the slots on the crossword puzzle
   * @param x - x axis
   * @param y - y axis
   */
  case class Slot(x: Int, y: Int) {

    def DOWN: Slot = this.copy(x, y + 1)

    def RIGHT: Slot = this.copy(x + 1, y)

    def availableSlots(puzzle: Puzzle, direction: String = ""): List[Slot] = {

      def look(slot: Slot) = Try { puzzle(slot.y)(slot.x) == '-' } getOrElse (false)

      direction match {
        case "" => List(DOWN, RIGHT) filter (look)
        case "down" => List(DOWN) filter (look)
        case "right" => List(RIGHT) filter (look)
      }
    }
  }
}

