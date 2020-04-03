import fpinscala.datastructure._

object Main {
  def main(args: Array[String]): Unit = {

    println("Test Map")
    println(Some(5).map(i => i + 1))

    println("Test flatMap")
    println(Some(5).flatMap(i => divide(i, 5)))

    println("Test getOrElse")
    var x = None: Option[Int]
    println(x.getOrElse(() => 6))

    println("Test orElse")
    println(x.orElse(y))

    println("Test Filter")
    println(Some(5).filter(a => a == 0))
  }

  def divide(a: Int, b: Int): Option[Int] = b match {
    case 0 => None
    case _ => Some(a / b)
  }

  def y(): Option[Int] = Some(45)
}
