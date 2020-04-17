case class Gen[A](sample: State.State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen.flatten(Gen[Gen[B]](RNG.map(this.sample)(f)))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(i => Gen.listOfN(i, this))
  }
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen[Int](RNG.withinRange(start, stopExclusive))
  }

  def unit[A](a: => A): Gen[A] = {
    Gen[A](RNG.unit(a))
  }

  def boolean: Gen[Boolean] = {
    Gen[Boolean](
      RNG.map(RNG.withinRange(0, 1))(a => if (a == 1) true else false)
    )
  }

  def flatten[A](a: Gen[Gen[A]]): Gen[A] = {
    Gen[A](rng => {
      val (g, rng2) = a.sample(rng)
      g.sample(rng2)
    })
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen[List[A]](RNG.sequence(List.fill(n)(g.sample)))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    val boolG = boolean
    Gen[A](rng => {
      val (b, rng2) = boolG.sample(rng)
      b match {
        case true  => g1.sample(rng2)
        case false => g2.sample(rng2)
      }
    })
  }

  // def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {

  // }
}

// trait Prop {
//   def check: Either[(Prop.FailedCase, Prop.SuccessCount), Prop.SuccessCount]
// }
case class Prop(run: (Prop.TestCases, RNG) => Prop.Result)
object Prop {
  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount)
      extends Result {
    def isFalsified = true
  }
  type SuccessCount = Int
  type TestCases = Int
  type Result = Option[(Prop.FailedCase, Prop.SuccessCount)]
  type FailedCase = String

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng)
      .zip(Stream.from(0))
      .take(n)
      .map {
        case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test ase: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTraec.mkString("\n")}"

}
