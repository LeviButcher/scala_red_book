trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0XFFFFFFFFFFFFL;
    val nextRNG = SimpleRNG(newSeed);
    val n = (newSeed >>> 16).toInt;
    (n, nextRNG)
  }
}

object RNG {
  type Rand[+A] = State.State[RNG, A]

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a, b), rng3)
      }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        rng => (mod, rng)
      else nonNegativeLessThan(n)
    })

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, rng2) = rng.nextInt
    x match {
      case Int.MinValue => (0, rng2)
      case _ if x < 0   => (-x, rng2)
      case _            => (x, rng2)
    }
  }
  def double(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(i => 1 / i.toDouble)(rng)
  }

  def int(rng: RNG): (Int, RNG) =
    rng.nextInt

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    both(int, double)(rng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    both(double, int)(rng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, rng2) = RNG.double(rng);
    val (dd, rng3) = RNG.double(rng2);
    val (ddd, rng4) = RNG.double(rng3);
    ((d, dd, ddd), rng4)
  }

//   def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
//        Stream.unfold()
//   }
}
