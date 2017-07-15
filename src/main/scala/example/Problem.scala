//package example
//
///**
//  *
//  */
//class Problem extends ProblemDef {
//
//  def createGrid(grid: String): Grid  =
//    grid.split("\\n").foldLeft(Vector.empty[Vector[Char]]){ (acc, next) =>
//      acc.:+(next.split(" ").map (_.charAt(0)).toVector)
//    }
//
//  //def isLegalPos(grid: Grid) = Try{ grid()}
//}
//
//
//trait ProblemDef{
//
//  type Grid = Vector[Vector[Char]]
//
//  type Neighbors = List[Pos]
//
//}
//
////case class Pos(x: Int, y: Int) {
////  def grabNeighbors(grid: Vector[Vector[Char]]): List[Pos] = {
////    val down = grid(x)(y + 1)
////    val left = grid(x - 1)(y)
////    val right = grid(x + 1)(y)
////    val up = grid(x)(y - 1)
////  }
////}
//
//
//class Solver extends ProblemDef{
////  def findNeighbors(pos: Pos, grid: Grid): Neighbors = {
////    val c = grid(pos.x)(pos.y)
////    pos.grabNeighbors(grid)
////  }
//}
