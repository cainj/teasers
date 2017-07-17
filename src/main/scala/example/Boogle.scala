package example

import scala.util.{Failure, Success, Try}


/**
  * Given a dictionary, a method to do lookup in dictionary and a M x N board where every cell has one character.
  * Find all possible words that can be formed by a sequence of adjacent characters.
  * Note that we can move to any of 8 adjacent characters, but a word should not have multiple instances of same cell.
  */
object Boggle {

  type Boggle = Array[Array[Char]]

  type Words = Array[String]

  type Square = (Pos, Char)

  type Path = List[Square]

  /**
    *
    * @param words  list of words i.e. -> {"GEEKS", "FOR", "QUIZ", "GO"};
    * @param boggle The boggle board game example below
    *                 {{'G','I','Z'},
    *                 {'U','E','K'},
    *                 {'Q','S','E'}};
    * @return
    */
  def solve(words: Words, boggle: Boggle) ={
    val pathsFromStart =
      (for{
      i <- words.indices
      starts <- findStartingPositions(words(i).head, boggle) map { m => (m, words(i))}
      found <- createStreams(starts._2.tail, boggle, List(starts._1), Set.empty)
    } yield found.mkString)

    pathsFromStart
  }

  /**
    * Creates the trail of letters to create the word
    *
    * @param word The word trying to solve
    * @param boggle The boggle board
    * @param path The stream of characters
    * @param explored The squares already visited.
    * @return
    */
  def createStreams(word: String, boggle: Boggle, path: Path, explored: Set[Square]): Stream[List[Char]] = {
    if(word != "")
      path match {
        case Nil => Stream(path map {_._2})
        case head :: tail =>
          (for {
            (pos, letter) <- head._1.validLetters(word.head, boggle) diff explored
            if(letter == word.head)
          } yield createStreams(word.tail, boggle, (pos, letter) :: path, explored + (pos -> letter))).flatten.toStream
      }
    else Stream(path.reverse map {_._2})
  }

  /**
    * Finds the starting positions
    * @param c Char to find
    * @param boggle The boggle
    * @return
    */
  def findStartingPositions(c: Char, boggle: Boggle): Seq[Square] = {
    for{
      i <- boggle.indices
      currArray = boggle(i)
      j <- currArray.indices
      if(c == currArray(j))
    } yield {
      (Pos(i, j), c)
    }
  }



  def main(args: Array[String]): Unit = {

    //Should return GEEKS and QUIZ
    println(solve(Array("GEEKS", "FOR", "QUIZ", "GO"), Array(Array('G','I','Z'), Array('U','E','K'), Array('Q','S','E'))))

    //Should find SLIP, SOUL, and SOUP
    println(solve(Array("POOL", "POOR", "SOUL", "SOUP", "SLIP"), Array(Array('P','I','O'), Array('O','U','L'), Array('R','S','O'))))
  }

  case class Pos(x: Int, y: Int) {

    def UP = Pos(this.x, this.y - 1)

    def DOWN = Pos(this.x, this.y + 1)

    def LEFT = Pos(this.x - 1, this.y)

    def RIGHT = Pos(this.x + 1, this.y)

    def UP_RIGHT = Pos(this.x + 1, this.y + 1)

    def DOWN_RIGHT = Pos(this.x + 1, this.y - 1)

    def UP_LEFT = Pos(this.x - 1, this.y + 1)

    def DOWN_LEFT = Pos(this.x - 1, this.y - 1)

    def Moves = List(UP, DOWN, LEFT, RIGHT, UP_RIGHT, DOWN_RIGHT, UP_LEFT, DOWN_LEFT)

    def grabAvailableLetters(boggle: Boggle): Set[Square] =
      Moves.foldLeft(Set.empty[(Pos, Char)]) { (accum, p) =>
        Try {
          boggle(p.x)(p.y)
        } match {
          case Failure(_) => accum
          case Success(x) => accum + (p -> x)
        }
      }

    def validLetters(c: Char, boggle: Boggle): Set[Square] = grabAvailableLetters(boggle) filter{ c == _._2}
  }
}

