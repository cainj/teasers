package ibotta

object UniqueItems {

  def main(args: Array[String]) {
    val parens = Array(7, 3, 2, 3, 4, 2, 7)
    println(findUniqueElements(parens))
  }

  def findUniqueElements[T](elements: Array[T]) = {
    elements.foldLeft(Map.empty[T, T]) {
      (map, next) => map + (next -> next)
    }.keys
  }
}
