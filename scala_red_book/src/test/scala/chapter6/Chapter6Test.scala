class Chapter6Test extends org.scalatest.FunSuite {

  test("nonNegativeInt") {
    assert(RNG.nonNegativeInt(SimpleRNG(-52))._1 > 0)
  }

  test("double") {
    assert(RNG.double(SimpleRNG(5))._1 < 1)
  }
}
