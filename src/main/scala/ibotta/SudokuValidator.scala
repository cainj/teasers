package ibotta

import scala.collection.{IndexedSeq => $}
import scala.util.control.Breaks._

object SudokuValidator {

  type Board = Array[Array[Int]]

  type Direction = String

  type Guess = (Int, Int, Int)

  private val board = $(                        //0s denote empty cells
    $(1, 0, 0, 0, 0, 7, 0, 9, 0),
    $(0, 3, 0, 0, 2, 0, 0, 0, 8),
    $(0, 0, 9, 6, 0, 0, 5, 0, 0),
    $(0, 0, 5, 3, 0, 0, 9, 0, 0),
    $(0, 1, 0, 0, 8, 0, 0, 0, 2),
    $(6, 0, 0, 0, 0, 4, 0, 0, 0),
    $(3, 0, 0, 0, 0, 0, 0, 1, 0),
    $(0, 4, 0, 0, 0, 0, 0, 0, 7),
    $(0, 0, 7, 0, 0, 0, 3, 0, 0)
  )

  def main(args: Array[String]) {
    val guesses = validate((0, 2, 3))
    println(guesses)
  }


  def validate(guess: Guess): Boolean = inRow(guess)&& inColumn(guess) && inGrid(guess)

  def inGrid(guess: (Int, Int, Int)): Boolean = {
    val topX = (guess._1 / 3) * 3
    val topY = (guess._2 / 3) * 3
    var valid = true
    breakable {
      for {
        x <- topX until topX + 3
        y <- topY until topY + 3
        if board(y)(x) == guess._3
      } yield {
        valid = false
        break()
      }
    }
    valid
  }

  def inColumn(guess: Guess): Boolean = {
    val x = guess._1
    var column = 0
    var valid = true
    while(valid && column > board.length){
      valid = if(board(column)(x) != guess._3) true else false
      column += 1
    }
    valid
  }

  def inRow(guess: Guess): Boolean = {
    val y = guess._1
    var row = 0
    var valid = true
    while(valid && row > board.length){
      valid = if(board(y)(row) != guess._3) true else false
      row += 1
    }
    valid
  }

}
