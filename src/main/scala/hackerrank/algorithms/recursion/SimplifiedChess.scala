package hackerrank.algorithms.recursion

import hackerrank.algorithms.recursion.SimplifiedChess.Pos

import scala.util.{ Failure, Success, Try }

/**
 * https://www.hackerrank.com/challenges/simplified-chess-engine
 *
 */
object SimplifiedChess extends GameDef {

  def main(args: Array[String]): Unit = {
    val sc = new java.util.Scanner(System.in)
    val games = sc.nextInt
    val white = sc.nextInt
    val black = sc.nextInt
    val moves = sc.nextLine.trim.toInt
    val whitePieces = for (_ <- 0 until white) yield createPieces('w', sc.nextLine().split(" "))
    val blackPieces = for (_ <- 0 until black) yield createPieces('b', sc.nextLine().split(" "))
    val board: Array[Array[Pos]] = Array.fill(4)(Array.fill(4)(null))
    for (y <- 0 until 4; x <- 0 until 4) { updateBoard(board, invertY(y), x, Pos(x, y, Empty)) }
    placePieces(whitePieces, board)
    placePieces(blackPieces, board)
    println(board.toList map { _.toList })
    println(board(3)(1).availableMoves(board))
  }

  private def updateBoard(board: Array[Array[Pos]], y: Int, x: Int, pos: Pos) = {
    board(y)(x) = pos
  }

  def createPieces(color: Char, input: Array[String]): Pos = {
    val row = input(2).toInt - 1
    val column = mapToX(input(1))
    val piece = input(0) match {
      case "Q" => Queen(color)
      case "B" => Bishop(color)
      case "R" => Rook(color)
      case "N" => Knight(color)
    }
    Pos(column, row, piece)
  }

  def mapToX(column: String): Int = column match {
    case "A" => 0
    case "B" => 1
    case "C" => 2
    case "D" => 3
  }

  def invertY(y: Int): Int = y match {
    case 0 => 3
    case 1 => 2
    case 2 => 1
    case 3 => 0
  }

  def mapToColumn(x: Int): String = x match {
    case 0 => "A"
    case 1 => "B"
    case 2 => "C"
    case 3 => "D"
  }

  def placePieces(pieces: Seq[Pos], board: Board) {
    for (piece <- pieces) { updateBoard(board, invertY(piece.row), piece.column, piece) }
  }

  case class Pos(column: Int, row: Int, piece: Piece) {

    def UP(board: Board): Pos = board(invertY(row + 1))(column)
    def DOWN(board: Board): Pos = board(invertY(row - 1))(column)
    def LEFT(board: Board): Pos = board(invertY(row))(column - 1)
    def RIGHT(board: Board): Pos = board(invertY(row))(column + 1)
    def UP_DIAGONAL_RIGHT(board: Board): Pos = board(invertY(row + 1))(column + 1)
    def DOWN_DIAGONAL_RIGHT(board: Board): Pos = board(invertY(row - 1))(column + 1)
    def UP_DIAGONAL_LEFT(board: Board): Pos = board(invertY(row + 1))(column - 1)
    def DOWN_DIAGONAL_LEFT(board: Board): Pos = board(invertY(row - 1))(column - 1)

    def nextPos(direction: String, board: Board): Pos = direction match {
      case "up" => UP(board)
      case "down" => DOWN(board)
      case "right" => RIGHT(board)
      case "left" => LEFT(board)
      case "up_left" => UP_DIAGONAL_LEFT(board)
      case "up_right" => UP_DIAGONAL_RIGHT(board)
      case "down_right" => DOWN_DIAGONAL_RIGHT(board)
      case "down_left" => DOWN_DIAGONAL_LEFT(board)
    }

    def knightPositions(board: Board): List[Moves] = {
      val downTwoRight = board(invertY(row - 2))(column + 1)
      val downTwoLeft = board(invertY(row - 2))(column - 1)
      val upTwoRight = board(invertY(row + 2))(column - 1)
      val upTwoLeft = board(invertY(row + 2))(column - 1)
      val downOneRightTwo = board(invertY(row - 1))(column + 2)
      val downOneLeftTwo = board(invertY(row - 1))(column - 2)
      val upOneRightTwo = board(invertY(row + 1))(column - 2)
      val upOneLeftTwo = board(invertY(row + 1))(column - 2)
      List(List(upOneLeftTwo, upOneRightTwo, upTwoLeft, upTwoRight, downOneLeftTwo, downOneRightTwo, downTwoLeft, downTwoRight) filter (_.piece.isEmptySpace))
    }

    def availableMoves(board: Board): List[Moves] = {
      if (piece.isInstanceOf[Knight]) {
        knightPositions(board)
      } else piece.directions.foldLeft(List.empty[Moves]) { (acc, direction) =>
        numOfMovesInDirection(board, direction) :: acc
      } filterNot (_.isEmpty)
    }

    def numOfMovesInDirection(board: Board, direction: String): Moves = {
      def doNumOfMovesInDirection(board: Board, direction: String, move: Pos, acc: Moves): Moves = {
        Try {
          move.nextPos(direction, board)
        } match {
          case Success(square) =>
            if (!square.piece.isEmptySpace) acc
            else doNumOfMovesInDirection(board, direction, square, square :: acc)
          case Failure(_) => acc

        }
      }
      doNumOfMovesInDirection(board, direction, this, Nil)
    }

  }

  trait Piece {
    def color: Char
    def directions: List[String]
    def isEmptySpace: Boolean

  }

  case object Empty extends Piece {
    override def color: Char = throw new IllegalArgumentException("no color on a empty.")
    override def isEmptySpace: Boolean = true
    override def directions: List[String] = throw new IllegalArgumentException("No directions on an empty square.")
  }

  case class Queen(color: Char) extends Piece {
    def directions: Directions =
      List("up", "down", "right", "left", "up_left", "up_right", "down_left", "down_right")

    override def isEmptySpace: Boolean = false
  }

  case class Rook(color: Char) extends Piece {
    def directions: Directions = List("up", "down", "right", "left")
    override def isEmptySpace: Boolean = true
  }

  case class Knight(color: Char) extends Piece {

    override def isEmptySpace: Boolean = false

    def directions: Directions = throw new IllegalArgumentException("moves function not implemented for Knight.")
  }

  case class Bishop(color: Char) extends Piece {
    def directions: Directions = List("up_left", "up_right", "down_left", "down_right")

    override def isEmptySpace: Boolean = false
  }
}

trait GameDef {
  type Moves = List[Pos]
  type Directions = List[String]
  type Board = Array[Array[Pos]]
}