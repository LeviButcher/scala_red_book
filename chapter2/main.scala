import Fib.fib
import IsSorted.isSorted

object Main {
  def curry [A, B, C] (f: (A, B) => C): A => (B => C) =
    (a: A) =>
      (b: B) =>
        f(a, b)

  def uncurry [A, B, C] (f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose [A, B, C] (f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    println(fib(8))
    var arr = Array(1,2,3,4,8)
    println(isSorted(arr, (a:Int, b:Int) => b > a))
  }
}
