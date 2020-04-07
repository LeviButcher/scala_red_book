class Chapter5Test extends org.scalatest.FunSuite {
  test("toList") {
    var x = Stream(1, 2, 3).toList
    var y = List(1, 2, 3)
    assert(x equals y)
  }

  test("Take") {
    var x = Stream(1, 2, 4).take(2);
    var y = Stream(1, 2)
    assert(x.headOption == y.headOption)
  }

  test("Drop") {
    var x = Stream(1, 2, 4).drop(2);
    var y = Stream(4)
    assert(x.headOption equals y.headOption)
  }

  var earlyTermination = (i: Int) => {
    println("Early!")
    i == 1
  }

  test("takeWhile") {
    var x = Stream(1, 1, 1, 2, 5)
    var y = List(1, 1, 1)
    assert(x.takeWhile(earlyTermination).toList equals y)
  }

  test("forAll") {
    var x = Stream(1, 1, 2, 1)
    assert(!x.forAll(i => i == 1))
  }
}
