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
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, rng2) = rng.nextInt
    x match {
      case Int.MinValue => (0, rng2)
      case _ if x < 0   => (-x, rng2)
      case _            => (x, rng2)
    }
  }
  def double(rng: RNG): (Double, RNG) = {
    val (x, rng2) = rng.nextInt
    val xx = 1 / x.toDouble
    xx match {
      case _ if x < 0 => (-xx, rng2)
      case _          => (xx, rng2)
      case 1          => (0, rng2)
    }
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (x, rng2) = rng.nextInt;
    val (d, rng3) = RNG.double(rng2);
    ((x, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (x, rng2) = rng.nextInt;
    val (d, rng3) = RNG.double(rng2);
    ((d, x), rng3)
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
