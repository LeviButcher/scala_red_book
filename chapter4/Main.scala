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

    println("Test variance")
    println(variance(Seq(1, 2, 3))) // 2/3
  }

  def divide(a: Double, b: Double): Option[Double] = b match {
    case 0 => None
    case _ => Some(a / b)
  }

  def y(): Option[Int] = Some(45)

  def variance(xs: Seq[Double]): Option[Double] = {
    // var m = divide(xs.sum, xs.length);
    // var squaredSum = xs
    //   .map(x => m.map(a => Math.pow(x - a, 2)))
    //   .reduce(
    //     (acc, curr) => Some(acc.getOrElse(0.0) + curr.getOrElse(0.0))
    //   );

    // squaredSum.flatMap(s => divide(s, xs.length))

    mean(xs) flatMap (m => mean(xs.map(x => Math.pow(x - m, 2))))
  }

  def mean(xs: Seq[Double]): Option[Double] = divide(xs.sum, xs.length)
}
