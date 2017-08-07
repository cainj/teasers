package hackerrank.algorithms.recursion

/**
 * https://www.hackerrank.com/challenges/simplified-chess-engine
 *
 */
object SimplifiedChess {
  type Board = Array[Array[Piece]]

  def main(args: Array[String]): Unit = {
    val sc = new java.util.Scanner(System.in)
    val games = sc.nextInt
    val white = sc.nextInt
    val black = sc.nextInt
    val moves = sc.nextLine.trim.toInt
    val whitePieces = for (i <- 0 until white) yield createPieces('w', sc.nextLine().split(" "))
    val blackPieces = for (i <- 0 until black) yield createPieces('b', sc.nextLine().split(" "))
    val board: Array[Array[Piece]] = Array.fill(4)(Array.fill(4)(null))
    for (y <- 0 until 4; x <- 0 until 4) {
      board(y)(x) = Empty(y, x)
    }
    placePieces(whitePieces, board)
    placePieces(blackPieces, board)
    println(board.toList map { _.toList })
  }

  def createPieces(color: Char, input: Array[String]): Piece = {
    val row = input(2).toInt
    val column = mapToX(input(1))
    input(0) match {
      case "Q" => Queen(row, column, color)
      case "B" => Bishop(row, column, color)
      case "R" => Rook(row, column, color)
      case "N" => Knight(row, column, color)
    }
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

  def placePieces(pieces: Seq[Piece], board: Board) {
    for (piece <- pieces) { board(piece.row)(piece.column) = piece }
  }

  trait Piece {
    def row: Int
    def column: Int
    def color: Char
    protected def moves = ???
    def availableMoves = ???
  }

  case class Empty(row: Int, column: Int) extends Piece {
    def color = throw new IllegalArgumentException("no color with on an empty piece.")
  }

  case class Queen(row: Int, column: Int, color: Char) extends Piece
  case class Rook(row: Int, column: Int, color: Char) extends Piece
  case class Knight(row: Int, column: Int, color: Char) extends Piece
  case class Bishop(row: Int, column: Int, color: Char) extends Piece
}
