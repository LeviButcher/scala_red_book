class Chapter10 extends org.scalatest.FunSuite {
  test("Monoid law test") {
    val gen = Gen(RNG.nonNegativeLessThan(50))
    val rng = SimpleRNG.apply(1000);

    assert(
      Monoid.monoidLaws(Monoid.intAddition, gen).run(50, rng) == Prop.Passed
    );

    val gen2 = gen.map(a => Part("Lorem", 2, "isp"))
    assert(
      Monoid
        .monoidLaws(Monoid.wcMonoid, gen2)
        .run(50, rng) == Prop.Passed
    );

    val gen3 = gen.map(x => Some(x))
    assert(
      Monoid
        .monoidLaws(Monoid.optionMonoid[Int], gen3)
        .run(50, rng) == Prop.Passed
    );

  }
}
