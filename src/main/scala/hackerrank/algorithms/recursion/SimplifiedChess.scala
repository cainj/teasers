package hackerrank.algorithms.recursion

import java.util.Scanner

import hackerrank.algorithms.recursion.SimplifiedChess.{ Move, Pos }

import scala.util.{ Failure, Random, Success, Try }

/**
 * https://www.hackerrank.com/challenges/simplified-chess-engine
 *
 */
object SimplifiedChess {

  type Moves = List[Move]
  type Directions = List[String]
  type Board = Array[Array[Pos]]
  type Pieces = Seq[Pos]

  def main(args: Array[String]): Unit = {
    val sc = new java.util.Scanner(System.in)
    val games = sc.nextInt
    val boards = for {
      _ <- 0 until games
    } yield createGames(sc)
    boards foreach { case (board, maxMoves) => play(board, maxMoves) }
  }

  private def createGames(sc: Scanner): (Board, Int) = {
    val white = sc.nextInt
    val black = sc.nextInt
    val maxMoves = sc.nextLine.trim.toInt
    val whitePieces = for (_ <- 0 until white) yield createPiece('w', sc.nextLine().split(" "))
    val blackPieces = for (_ <- 0 until black) yield createPiece('b', sc.nextLine().split(" "))
    val board: Array[Array[Pos]] = Array.fill(4)(Array.fill(4)(null))
    for (y <- 0 until 4; x <- 0 until 4) {
      updateBoard(board, invertY(y), x, Pos(x, y, Empty))
    }
    placePieces(whitePieces, board)
    placePieces(blackPieces, board)
    printBoard(board)
    (board, maxMoves)
  }

  def printBoard(board: Board): Unit = {
    println(board.toList map { _.toList })
  }

  def possibleMoves(board: Board, turn: Char): (Char, List[Board]) = {
    println(s"Whose turn is it anyway $turn")
    //printBoard(board)
    val moves = board.foldLeft(List.empty[Move]) { (acc, row) =>
      val pieces = row.filter(p => p.piece.color == turn)
      acc ++ pieces.flatMap { _.availableMoves(board) }
    }
    val boards = moves map { move => makeMove(board, move) }
    //println(s"Moves - $moves")
    (if (turn == 'w') 'b' else 'w', boards)
  }

  def play(game: Board, maxMoves: Int) {

    def miniMax(board: Board, turn: Char, maxMoves: Int): Int = {
      if (gameOver(board) || 0 == maxMoves) {
        println("Game Over  " + score(board))
        println(printBoard(board))
        score(board)
      } else {
        val (color, moves) = possibleMoves(board, turn)
        if (color == 'w')
          moves.foldLeft(-1)((max: Int, board: Board) => Math.max(max, miniMax(board, color, maxMoves - 1)))
        else {
          moves.foldLeft(1)((min: Int, board: Board) => Math.min(min, miniMax(board, color, maxMoves - 1)))
        }
      }
    }

    val (color, moves) = possibleMoves(game, 'w')
    val valuesAndMoves = moves.foldLeft(Vector[(Int, Board)]())((acc, b) => acc :+ (miniMax(b, color, maxMoves - 1), b))

    println(valuesAndMoves)
    // used for reverse sorting
    def Desc[T: Ordering] = implicitly[Ordering[T]].reverse

    val valesAndMovesSortedByValue = valuesAndMoves.sortBy(tup => (tup._1))(Desc)

    val maxValue = valesAndMovesSortedByValue(0)._1
    println(if (maxValue == 1) "YES" else "NO")
  }

  def gameOver(board: Board): Boolean = {
    findQueens(board).length == 1
  }

  def findQueens(board: Board) = {
    board.collect {
      case row if row.exists(_.piece.isInstanceOf[Queen]) =>
        row.filter(_.piece.isInstanceOf[Queen])
    }.flatten
  }

  def score(board: Board): Int = {
    val queens = findQueens(board)
    if (queens.length == 2) 0
    else {
      val queen = queens.headOption getOrElse (throw new IllegalStateException("Must have at least one queen on the board"))
      if (queen.piece.color == 'w') 1 else -1
    }
  }

  private def updateBoard(board: Array[Array[Pos]], y: Int, x: Int, pos: Pos) {
    board(y)(x) = pos
  }

  private def makeMove(board: Board, move: Move): Board = {
    val newBoard = board map { _.clone() }
    newBoard(invertY(move.to.row))(move.to.column) = Pos(move.to.column, move.to.row, move.from.piece)
    newBoard(invertY(move.from.row))(move.from.column) = Pos(move.from.column, move.from.row, Empty)
    newBoard
  }

  def createPiece(color: Char, input: Array[String]): Pos = {
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

  case class Move(from: Pos, to: Pos)

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
      val downTwoRight = () => board(invertY(row - 2))(column + 1)
      val downTwoLeft = () => board(invertY(row - 2))(column - 1)
      val upTwoRight = () => board(invertY(row + 2))(column + 1)
      val upTwoLeft = () => board(invertY(row + 2))(column - 1)
      val downOneRightTwo = () => board(invertY(row - 1))(column + 2)
      val downOneLeftTwo = () => board(invertY(row - 1))(column - 2)
      val upOneRightTwo = () => board(invertY(row + 1))(column + 2)
      val upOneLeftTwo = () => board(invertY(row + 1))(column - 2)
      List(List(upOneLeftTwo, upOneRightTwo, upTwoLeft, upTwoRight, downOneLeftTwo, downOneRightTwo, downTwoLeft, downTwoRight) map {
        f => Try { f() }
      } collect { case Success(x) => Move(this, x) })
    }

    def availableMoves(board: Board): Moves = {
      (if (piece.isInstanceOf[Knight]) {
        knightPositions(board)
      } else piece.directions.foldLeft(List.empty[Moves]) { (acc, direction) =>
        numOfMovesInDirection(board, direction) :: acc
      } filterNot (_.isEmpty)).flatten
    }

    def numOfMovesInDirection(board: Board, direction: String): Moves = {
      def doNumOfMovesInDirection(board: Board, direction: String, move: Pos, acc: Moves): Moves = {
        Try {
          move.nextPos(direction, board)
        } match {
          case Success(square) =>
            if (square.piece.isEmptySpace || square.piece.isInstanceOf[Queen] && square.piece.asInstanceOf[Queen].color != this.piece.color)
              doNumOfMovesInDirection(board, direction, square, Move(this, square) :: acc)
            else acc
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
    override def color: Char = ' '
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
