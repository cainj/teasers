package ibotta

object ParenthesisValidator {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val parens = sc.nextLine()
    println(validateRecursion(parens))
  }

  def validate(line: String): Boolean = {
    val chars = line.toCharArray
    var count = 0

    for(i <- chars.indices){
      if(chars(i) == '(')
        count += 1
      else if(chars(i) == ')')
        count -= 1
      if(count < 0)
        return false
    }
    count == 0
  }

  def validateRecursion(line: String, count: Int = 0): Boolean = {
    if(count < 0 || (count != 0 && line.isEmpty)) false
    else if(count == 0 && line.isEmpty) true
    else {
      val c = line.head
      if(c == '(') validateRecursion(line.tail, count + 1)
      else if(c == ')') validateRecursion(line.tail, count - 1)
      else validateRecursion(line.tail, count)
    }
  }

}
