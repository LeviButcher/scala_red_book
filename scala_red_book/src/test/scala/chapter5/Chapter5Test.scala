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

  test("headOption") {
    var x = Stream(1, 2, 3)
    assert(x.headOption == Some(1))
  }

  test("Map") {
    var x = Stream(1, 2, 3)
    assert(x.map(i => i + 3).toList == List(4, 5, 6))
  }

  test("Filter") {
    var x = Stream(1, 2, 1, 3)
    assert(x.filter(i => i == 1).toList == List(1, 1))
  }

  test("Append") {
    var x = Stream(1, 2)
    assert(x.append(1).toList == List(1, 2, 1))
  }

  test("flatmap") {
    var x = Stream(Stream(1), Stream(2), Stream(3))
    assert(x.flatMap(i => i).toList == List(1, 2, 3))
  }

  test("Constant") {
    var x = Stream.constant(1)
    assert(x.take(3).toList == List(1, 1, 1))
  }

  test("from") {
    var x = Stream.from(5)
    assert(x.take(3).toList == List(5, 6, 7))
  }

  test("fibs") {
    var x = Stream.fibs()
    assert(x.take(5).toList == List(0, 1, 1, 2, 3))
  }

  test("ZipWith") {
    var x = Stream(1, 2, 3);
    assert(x.zipWith(Stream(1, 2, 3))((a, b) => a + b).toList == List(2, 4, 6))
  }

  test("ZipAll") {
    var x = Stream(1, 2, 34, 5, 64)
    var y = Stream(1, 2)
    println(x.zipAll(y).toList)
  }
}
