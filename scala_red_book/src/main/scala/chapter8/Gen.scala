import Prop.Passed
import Prop.Falsified
case class Gen[+A](sample: State.State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen.flatten(Gen[Gen[B]](RNG.map(this.sample)(f)))
  }

  def map[B](f: A => B): Gen[B] =
    Gen(State.map(sample)(f))

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(i => Gen.listOfN(i, this))
  }

  def unsized: SGen[A] = {
    SGen[A](x => this)
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

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(x => listOfN(x, g))
  }

}

case class Prop(run: (Prop.TestCases, RNG) => Prop.Result) {
  def &&(p: Prop): Prop = {
    Prop((t, rng) => {
      run(t, rng) match {
        case Passed => p.run(t, rng)
        case x      => x
      }
    })
  }
  def ||(p: Prop): Prop = {
    Prop((t, rng) => {
      run(t, rng) match {
        case Falsified(failure, successes) => p.run(t, rng)
        case x                             => x
      }
    })
  }
}
object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type FailedCase = String

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

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng)
      .zip(Stream.from(0))
      .take(n)
      .map(x => {
        x match {
          case (a, i) =>
            try {
              if (f(a)) Passed else Falsified(a.toString, i)
            } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        }
      })
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test ase: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen { g(_) map f }

}
