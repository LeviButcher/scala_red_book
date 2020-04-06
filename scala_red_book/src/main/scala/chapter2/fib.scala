// a comment
object Fib {
  def fib(n: Int): Int = {
    @annotation.tailrec
    // change type to a:Int b:int c:Int
    def tailFib(n: Int, a: Int = 0, b: Int = 1): Int =
      if(n == 0 || n == 1) b
      else tailFib(n - 1, b, a + b)

    tailFib(n)
  }
}
