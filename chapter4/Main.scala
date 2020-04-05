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

    println("Test Map2")
    println(map2(Some(2), Some(3))((a, b) => a + b))

    println("Test Sequence")
    println(sequence(List(Some(5), Some(6), Some(11))))
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

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aVal => b.map(bVal => f(aVal, bVal)))
  }

  // My OG Sequence, ruins list order :(
  // def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
  //   case Nil     => Some(Nil)
  //   case x :: xs => sequence(xs).flatMap(a => x.map(b => a :+ b))
  // }

  // The scala books, that doesn't ruin the order
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil    => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }
}
