case class Gen[A](sample: State.State[RNG, A])

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

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen[List[A]](RNG.sequence(List.fill(n)(g.sample)))
  }
}
